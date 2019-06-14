{******************************************************************************}
{*  UNIT      : Contains the class TWetlandValidator.                         *}
{*  AUTHOR    : Maurice Marinus                                               *}
{*  DATE      : 2006/08/11                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UWetlandValidator;

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
  UWetlandDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType,
  UWetland,
  VCL.Dialogs,
  UFileNames;

type
  TWetlandValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects;                  override;
    procedure DestroyMemberObjects;                 override;
    procedure OnEditControlEnter(Sender: TObject);  override;
    procedure OnEditControltExit(Sender: TObject);  override;

    procedure OnSelectPenaltyClick(Sender: TObject);
    procedure OnReservoirExistsClick(Sender: TObject);
    procedure OnSummaryIncludeClick(Sender: TObject);
    procedure OnViewGridClick(Sender: TObject);
    procedure OnViewGraphClick(Sender: TObject);
    procedure OnNewPenaltyClick(Sender: TObject);
    procedure OnSelectHistoricWaterlevelClick(Sender : TObject);

    procedure ValidateNodeNumber;
    procedure ValidateName;
    procedure ValidateStorageVolume;
    procedure ValidateInflowProportion;
    procedure ValidateOutflowProportion;
    procedure ValidateUpstreamThreshold;
    procedure ValidateInflowUpstreamNode;
    procedure ValidateOutflowDownstreamNode;
    procedure ValidateXCoord(AReservoirData: IReservoirData);
    procedure ValidateYCoord(AReservoirData: IReservoirData);

    procedure ValidateReservoirPenaltyStructure(AReservoir : IReservoirData);
    procedure ValidateReservoirPriority(AReservoir: IReservoirData);
    procedure ValidateRainCoef(AReservoir: IReservoirConfigurationData);
    procedure ValidateDamLevelFileName (AReservoir : IReservoirData);
    procedure SetGridGraphBtnState(AReservoir : IReservoirData);

    procedure UpdateNodeNumber;
    procedure UpdateName;
    procedure UpdateStorageVolume;
    procedure UpdateInflowProportion;
    procedure UpdateOutflowProportion;
    procedure UpdateUpstreamThreshold;
    procedure UpdateInflowUpstreamNode;
    procedure UpdateOutflowDownstreamNode;

    procedure UpdateResevoirPenaltyStructure;
    procedure UpdateResevoirPriority;
    procedure UpdateResevoirIncludeSummary;
    procedure UpdateDamLevelFileName;
    procedure UpdateReservoirExists;
    procedure UpdateRainCoef;
    procedure UpdateXCoord;
    procedure UpdateYCoord;

    procedure RePopulateDataViewer;
    procedure RePopulateNodes;
    procedure RepopulateHistoricWaterLevels;
  public
    function Initialise:          Boolean; override;
    function SaveState:           Boolean; override;
    function LanguageHasChanged:  Boolean; override;
    function StudyHasChanged:     Boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function WetlandDialog:       TWetlandDialog;

    procedure ClearDataViewer;    override;
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
  USelectChannelDialog,
  Math,
  UNetworkFeaturesData,
  UAbstractFileNamesObject,
  UReservoirPenaltyValidator,
  UReservoirPenaltyDialog;

{******************************************************************************}
{* TWetlandValidator                                                        *}
{******************************************************************************}

procedure TWetlandValidator.CreateMemberObjects;
const OPNAME = 'TWetlandValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TWetlandDialog.Create(FPanelOwner,FAppModules);

    with TWetlandDialog(FPanel) do
    begin
      NodeNumberEdit.OnEnter         := OnEditControlEnter;
      NameEdit.OnEnter               := OnEditControlEnter;
      StorageVolumeEdit.OnEnter      := OnEditControlEnter;
      InflowProportionEdit.OnEnter   := OnEditControlEnter;
      OutflowProportionEdit.OnEnter  := OnEditControlEnter;
      UpstreamThresholdEdit.OnEnter  := OnEditControlEnter;
      InflowCbx.OnEnter              := OnEditControlEnter;
      OutflowCbx.OnEnter             := OnEditControlEnter;

      NodeNumberEdit.OnExit          := OnEditControltExit;
      NameEdit.OnExit                := OnEditControltExit;
      StorageVolumeEdit.OnExit       := OnEditControltExit;
      InflowProportionEdit.OnExit    := OnEditControltExit;
      OutflowProportionEdit.OnExit   := OnEditControltExit;
      UpstreamThresholdEdit.OnExit   := OnEditControltExit;
      InflowCbx.OnExit               := OnEditControltExit;
      OutflowCbx.OnExit              := OnEditControltExit;

      NodeNumberEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('WetlandNodeNumber');
      NameEdit.FieldProperty               := FAppModules.FieldProperties.FieldProperty('WetlandName');
      StorageVolumeEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('StorageVolume');
      InflowProportionEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('InflowProportion');
      OutflowProportionEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('OutflowProportion');
      UpstreamThresholdEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('UpstreamThreshold');
      InflowCbx.FieldProperty              := FAppModules.FieldProperties.FieldProperty('DiversionChannelUpStreamNode');
      OutflowCbx.FieldProperty             := FAppModules.FieldProperties.FieldProperty('ReturnFlowChannelDownStreamNode');

      PenaltyStructureEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
      PenaltyStructureEdit.OnEnter       := OnEditControlEnter;
      PenaltyStructureEdit.OnExit        := OnEditControltExit;

      PriorityEdit.FieldProperty         := FAppModules.FieldProperties.FieldProperty('ReservoirPriority');
      PriorityEdit.OnEnter               := OnEditControlEnter;
      PriorityEdit.OnExit                := OnEditControltExit;

      SummaryIncludeChkBox.FieldProperty      := FAppModules.FieldProperties.FieldProperty('IncludeSummary');
      SummaryIncludeChkBox.CheckBoxIdentifier := giIncludeSummary;
      SummaryIncludeChkBox.OnEnter            := OnEditControlEnter;
      SummaryIncludeChkBox.OnClick            := OnSummaryIncludeClick;

      ReservoirExistsChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('StatusIndicator');
      ReservoirExistsChkBox.CheckBoxIdentifier := giIncludeSummary;
      ReservoirExistsChkBox.OnEnter        := OnEditControlEnter;
      ReservoirExistsChkBox.OnClick        := OnReservoirExistsClick;

      SelectPenaltyStruct.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyStruct');
      SelectPenaltyStruct.OnEnter     := OnEditControlEnter;
      SelectPenaltyStruct.OnExit      := OnEditControltExit;
      SelectPenaltyStruct.OnClick     := OnSelectPenaltyClick;

      RainCoeffEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('RainCoef');
      RainCoeffEdit.OnEnter       := OnEditControlEnter;
      RainCoeffEdit.OnExit        := OnEditControltExit;

      WetlandXCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('XCoord');
      WetlandXCoordEdit.OnEnter       := OnEditControlEnter;
      WetlandXCoordEdit.OnExit        := OnEditControltExit;

      WetlandYCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YCoord');
      WetlandYCoordEdit.OnEnter       := OnEditControlEnter;
      WetlandYCoordEdit.OnExit        := OnEditControltExit;

      DamLevelsFileNameCbx.FieldProperty     := FAppModules.FieldProperties.FieldProperty('DamLevelsFileName');
      DamLevelsFileNameCbx.OnEnter           := OnEditControlEnter;
      DamLevelsFileNameCbx.OnExit            := OnEditControltExit;

      DamLevelsFileNameSelectBtn.FieldProperty := FAppModules.fieldProperties.FieldProperty('PenaltyStruct');
      DamLevelsFileNameSelectBtn.OnEnter       := OnEditControlEnter;
      DamLevelsFileNameSelectBtn.OnExit        := OnEditControltExit;
      DamLevelsFileNameSelectBtn.OnClick       := OnSelectHistoricWaterlevelClick;
      DamLevelsFileNameSelectBtn.Enabled  := (FAppModules.User.UserRights in CUR_EditData) and
                                          (not FAppModules.StudyArea.ScenarioLocked);

      DamLevelsFileNameGridBtn.OnEnter  := OnEditControlEnter;
      DamLevelsFileNameGridBtn.OnExit   := OnEditControltExit;
      DamLevelsFileNameGridBtn.OnClick  := OnViewGridClick;

      DamLevelsFileNameGraphBtn.OnEnter  := OnEditControlEnter;
      DamLevelsFileNameGraphBtn.OnExit   := OnEditControltExit;
      DamLevelsFileNameGraphBtn.OnClick  := OnViewGraphClick;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.DestroyMemberObjects;
const OPNAME = 'TWetlandValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandValidator.Initialise: boolean;
const OPNAME = 'TWetlandValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandValidator.LanguageHasChanged: boolean;
const OPNAME = 'TWetlandValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.Wetland');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ClearDataViewer;
const OPNAME = 'TWetlandValidator.ClearDataViewer';
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
    with WetlandDialog do
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

procedure TWetlandValidator.PopulateDataViewer;
const OPNAME = 'TWetlandValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtWetlandAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TWetlandValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
   if (AFieldName = 'PenaltyStruct')  and
      (AOldValue = WetlandDialog.PenaltyStructureEdit.Text) then
      RePopulateDataViewer;
   if (AFieldName = 'ReservoirPriority')  and
      (AOldValue = WetlandDialog.PriorityEdit.Text) then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandValidator.StudyHasChanged: boolean;
const OPNAME = 'TWetlandValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TWetlandValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TWetlandValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with WetlandDialog do
    begin
      if((Sender = NodeNumberEdit) and (NodeNumberEdit.HasValueChanged ))then
        UpdateNodeNumber
      else
      if((Sender = NameEdit) and (NameEdit.HasValueChanged ))then
        UpdateName
      else
      if((Sender = StorageVolumeEdit) and (StorageVolumeEdit.HasValueChanged ))then
        UpdateStorageVolume
      else
      if((Sender = InflowProportionEdit) and (InflowProportionEdit.HasValueChanged ))then
        UpdateInflowProportion
      else
      if((Sender = OutflowProportionEdit) and (OutflowProportionEdit.HasValueChanged ))then
        UpdateOutflowProportion
      else
      if((Sender = UpstreamThresholdEdit) and (UpstreamThresholdEdit.HasValueChanged ))then
        UpdateUpstreamThreshold
      else
      if((Sender = InflowCbx) and (InflowCbx.HasValueChanged ))then
        UpdateInflowUpstreamNode
      else
      if((Sender = OutflowCbx) and (OutflowCbx.HasValueChanged ))then
        UpdateOutflowDownstreamNode
      else
      if ((sender = WetlandDialog.WetlandXCoordEdit) AND
       (WetlandDialog.WetlandXCoordEdit.HasValueChanged)) then
       UpdateXCoord
      else
      if ((sender = WetlandDialog.WetlandYCoordEdit) AND
       (WetlandDialog.WetlandYCoordEdit.HasValueChanged)) then
       UpdateYCoord
      else
      if ((Sender =  PenaltyStructureEdit) and (PenaltyStructureEdit.HasValueChanged))then
         UpdateResevoirPenaltyStructure
      else
      if ((Sender =  PriorityEdit) and (PriorityEdit.HasValueChanged)) then
         UpdateResevoirPriority
      else
      if ((sender = RainCoeffEdit) and (RainCoeffEdit.HasValueChanged)) then
         UpdateRainCoef
      else
      if ((sender = DamLevelsFileNameCbx) and (DamLevelsFileNameCbx.HasValueChanged)) then
      begin
        UpdateDamLevelFileName;
        RePopulateDataViewer;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.RePopulateDataViewer;
const OPNAME = 'TWetlandValidator.RePopulateDataViewer';
var
  LWetland : IWetland;
  lReservoirData    : IReservoirData;
  lReservoir        : IReservoirConfigurationData;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        NodeNumberEdit.SetFieldValue(LWetland.NodeNumber);
        NameEdit.SetFieldValue(LWetland.Name);
        StorageVolumeEdit.SetFieldValue(LWetland.StorageVolume);
        InflowProportionEdit.SetFieldValue(LWetland.InflowProportion);
        OutflowProportionEdit.SetFieldValue(LWetland.OutflowProportion);
        UpstreamThresholdEdit.SetFieldValue(LWetland.UpstreamThreshold);

        RePopulateNodes;
        if (LWetland.InflowChannel <> nil) then
        begin
          lReservoirData :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                               ReservoirList.ReservoirOrNodeByIdentifier[LWetland.InflowChannel.UpStreamNodeNumber];
          if (lReservoirData <> nil) then
          begin
            lReservoir := lReservoirData.ReservoirConfigurationData;
            InflowCbx.SetFieldIndex
              (InflowCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
          end;
        end;

        if (LWetland.OutflowChannel <> nil) then
        begin
          lReservoirData :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                               ReservoirList.ReservoirOrNodeByIdentifier[LWetland.OutflowChannel.DownStreamNodeNumber];
          if (lReservoirData <> nil) then
          begin
            lReservoir := lReservoirData.ReservoirConfigurationData;
            OutflowCbx.SetFieldIndex
              (OutflowCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
          end;
        end;

        lReservoirData :=  TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                             ReservoirList.ReservoirByIdentifier[LWetland.NodeNumber];
        if (lReservoirData <> nil) then
        begin
          PenaltyStructureEdit.SetFieldValue(lReservoirData.ReservoirConfigurationData.PenaltyStructIdentifier);
          PriorityEdit.SetFieldValue(lReservoirData.ReservoirConfigurationData.Priority);
          RainCoeffEdit.SetFieldValue(lReservoirData.ReservoirConfigurationData.RainCoef);
          SummaryIncludeChkBox.Checked := (UpperCase(Trim(lReservoirData.ReservoirConfigurationData.IncludeSummary)) = 'Y');
          ReservoirExistsChkBox.Checked := (lReservoirData.ReservoirConfigurationData.StatusIndicator = 1);
          WetlandXCoordEdit.SetFieldValue(lReservoirData.ReservoirConfigurationData.XCoord);
          WetlandYCoordEdit.SetFieldValue(lReservoirData.ReservoirConfigurationData.YCoord);

          RepopulateHistoricWaterLevels;
          SetGridGraphBtnState(lReservoirData);
        end;          
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandValidator.SaveState: boolean;
const OPNAME = 'TWetlandValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TWetlandValidator.DoContextValidation';
var
  lWetland    : IWetland;
  lReservoir  : IReservoirData;
begin
  try
    lWetland    := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    lReservoir  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[lWetland.NodeNumber];
    if (lWetland <> nil) then
    begin
      case AValidationType of
        dvtWetlandAll :
        begin
          ValidateNodeNumber;
          ValidateName;
          ValidateStorageVolume;
          ValidateInflowProportion;
          ValidateOutflowProportion;
          ValidateUpstreamThreshold;
          ValidateInflowUpstreamNode;
          ValidateOutflowDownstreamNode;
          if lReservoir <> nil then
          begin
            ValidateXCoord(lReservoir);
            ValidateYCoord(lReservoir);
            ValidateReservoirPenaltyStructure(lReservoir);
            ValidateReservoirPriority(lReservoir);
            ValidateRainCoef(lReservoir.ReservoirConfigurationData);
            ValidateDamLevelFileName(lReservoir);
          end;
        end;
        dvtWetlandNodeNumber          : ValidateNodeNumber;
        dvtWetlandName                : ValidateName;
        dvtWetlandStorageVolume       : ValidateStorageVolume;
        dvtWetlandInflowProportion    : ValidateInflowProportion;
        dvtWetlandOutflowProportion   : ValidateOutflowProportion;
        dvtWetlandUpstreamThreshold   : ValidateUpstreamThreshold;
        dvtWetlandInflowUpstreamNode  : ValidateInflowUpstreamNode;
        dvtWetlandOutflowDownstreamNode : ValidateOutflowDownstreamNode;
        dvtReservoirPenalty           : ValidateReservoirPenaltyStructure(lReservoir);
        dvtResPropPriority            : ValidateReservoirPriority(lReservoir);
        dvtResPropRainCoef            : ValidateRainCoef(lReservoir.ReservoirConfigurationData);
        dvtDamlevelFileName           : ValidateDamLevelFileName(lReservoir);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWetlandValidator.WetlandDialog: TWetlandDialog;
const OPNAME = 'TWetlandValidator.WetlandDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TWetlandDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateInflowProportion;
const OPNAME = 'TWetlandValidator.UpdateInflowProportion';
var
  LWetland : IWetland;
  LMessage : string;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        InflowProportionEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        InflowProportionEdit.FieldProperty.FieldName,
                        InflowProportionEdit.Text, LMessage) then
        begin
          LWetland.InflowProportion := StrToFloat(InflowProportionEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtWetlandInflowProportion);
        end
        else
          InflowProportionEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateName;
const OPNAME = 'TWetlandValidator.UpdateName';
var
  LWetland : IWetland;
  LMessage : string;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        NameEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        NameEdit.FieldProperty.FieldName,
                        NameEdit.Text, LMessage) then
        begin
          LWetland.Name := NameEdit.Text;
          RePopulateDataViewer;
          DoContextValidation (dvtWetlandName);
        end
        else
          NameEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateNodeNumber;
const OPNAME = 'TWetlandValidator.UpdateNodeNumber';
var
  LWetland : IWetland;
  LMessage : string;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        NodeNumberEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        NodeNumberEdit.FieldProperty.FieldName,
                        NodeNumberEdit.Text, LMessage) then
        begin
          LWetland.NodeNumber := StrToInt(NodeNumberEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtWetlandNodeNumber);
        end
        else
          NodeNumberEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateOutflowProportion;
const OPNAME = 'TWetlandValidator.UpdateOutflowProportion';
var
  LWetland : IWetland;
  LMessage : string;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        OutflowProportionEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        OutflowProportionEdit.FieldProperty.FieldName,
                        OutflowProportionEdit.Text, LMessage) then
        begin
          LWetland.OutflowProportion := StrToFloat(OutflowProportionEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtWetlandOutflowProportion);
        end
        else
          OutflowProportionEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateStorageVolume;
const OPNAME = 'TWetlandValidator.UpdateStorageVolume';
var
  LWetland : IWetland;
  LMessage : string;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        StorageVolumeEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        StorageVolumeEdit.FieldProperty.FieldName,
                        StorageVolumeEdit.Text, LMessage) then
        begin
          LWetland.StorageVolume := StrToFloat(StorageVolumeEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtWetlandStorageVolume);
        end
        else
          StorageVolumeEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateUpstreamThreshold;
const OPNAME = 'TWetlandValidator.UpdateUpstreamThreshold';
var
  LWetland : IWetland;
  LMessage : string;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        UpstreamThresholdEdit.FieldValidationError := '';
        if FAppModules.FieldProperties.ValidateFieldProperty(
                        UpstreamThresholdEdit.FieldProperty.FieldName,
                        UpstreamThresholdEdit.Text, LMessage) then
        begin
          LWetland.UpstreamThreshold := StrToFloat(UpstreamThresholdEdit.Text);
          RePopulateDataViewer;
          DoContextValidation (dvtWetlandUpstreamThreshold);
        end
        else
          UpstreamThresholdEdit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateInflowProportion;
const OPNAME = 'TWetlandValidator.ValidateInflowProportion';
var
  LWetland : IWetland;
  LMessage : WideString;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        InflowProportionEdit.ContextValidationError := '';
        if (not LWetland.Validate(LMessage, InflowProportionEdit.FieldProperty.FieldName)) then
          InflowProportionEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateName;
const OPNAME = 'TWetlandValidator.ValidateName';
var
  LWetland : IWetland;
  LMessage : WideString;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        NameEdit.ContextValidationError := '';
        if (not LWetland.Validate(LMessage, NameEdit.FieldProperty.FieldName)) then
          NameEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateNodeNumber;
const OPNAME = 'TWetlandValidator.ValidateNodeNumber';
var
  LWetland : IWetland;
  LMessage : WideString;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        NodeNumberEdit.ContextValidationError := '';
        if (not LWetland.Validate(LMessage, NodeNumberEdit.FieldProperty.FieldName)) then
          NodeNumberEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateOutflowProportion;
const OPNAME = 'TWetlandValidator.ValidateOutflowProportion';
var
  LWetland : IWetland;
  LMessage : WideString;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        OutflowProportionEdit.ContextValidationError := '';
        if (not LWetland.Validate(LMessage, OutflowProportionEdit.FieldProperty.FieldName)) then
          OutflowProportionEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateStorageVolume;
const OPNAME = 'TWetlandValidator.ValidateStorageVolume';
var
  LWetland : IWetland;
  LMessage : WideString;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        StorageVolumeEdit.ContextValidationError := '';
        if (not LWetland.Validate(LMessage, StorageVolumeEdit.FieldProperty.FieldName)) then
          StorageVolumeEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateUpstreamThreshold;
const OPNAME = 'TWetlandValidator.ValidateUpstreamThreshold';
var
  LWetland : IWetland;
  LMessage : WideString;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        UpstreamThresholdEdit.ContextValidationError := '';
        if (not LWetland.Validate(LMessage, UpstreamThresholdEdit.FieldProperty.FieldName)) then
          UpstreamThresholdEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateInflowUpstreamNode;
const OPNAME = 'TWetlandValidator.UpdateInflowUpstreamNode';
var
  lReservoir     : IReservoirConfigurationData;
  lReservoirNr   : integer;
  lWetland       : IWetland;
  lMessage       : string;
begin
  try
    lWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (lWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        lReservoir   := nil;
        lReservoirNr := -1;

        if (InflowCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(InflowCbx.Items.Objects[InflowCbx.ItemIndex]);
          lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
        end;
        if (lReservoir <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('DiversionChannelUpStreamNode',IntToStr(lReservoirNr), lMessage)) then
          begin
            if Assigned(lWetland.InflowChannel) then
            begin
              lWetland.InflowChannel.UpStreamNodeNumber := lReservoirNr;
              lReservoirNr := lWetland.InflowChannel.UpStreamNodeNumber;
            end;
            InflowCbx.SetFieldIndex(InflowCbx.Items.IndexOfObject(TObject(lReservoirNr)));
            DoContextValidation(dvtWetlandInflowUpstreamNode);
          end
          else
            InflowCbx.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateOutflowDownstreamNode;
const OPNAME = 'TWetlandValidator.UpdateOutflowDownstreamNode';
var
  lReservoir     : IReservoirConfigurationData;
  lReservoirNr   : integer;
  lWetland       : IWetland;
  lMessage       : string;
begin
  try
    lWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (lWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        lReservoir   := nil;
        lReservoirNr := -1;

        if (OutflowCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(OutflowCbx.Items.Objects[OutflowCbx.ItemIndex]);
          lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
        end;
        if (lReservoir <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty('ReturnFlowChannelDownStreamNode',IntToStr(lReservoirNr), lMessage)) then
          begin
            if Assigned(lWetland.OutflowChannel) then
            begin
              lWetland.OutflowChannel.DownStreamNodeNumber := lReservoirNr;
              lReservoirNr := lWetland.OutflowChannel.DownStreamNodeNumber;
            end;
            OutflowCbx.SetFieldIndex(OutflowCbx.Items.IndexOfObject(TObject(lReservoirNr)));
            DoContextValidation(dvtWetlandOutflowDownstreamNode);
          end
          else
            OutflowCbx.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateInflowUpstreamNode;
const OPNAME = 'TWetlandValidator.ValidateInflowUpstreamNode';
var
  lWetland  : IWetland;
begin
  try
    lWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (lWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        FErrorMessage := '';
        if (lWetland.Validate(FErrorMessage, 'DiversionUpstreamNode')) then
        begin
          InflowCbx.InValidationError := FALSE;
          InflowCbx.ShowErrorState(FALSE);
        end
        else
        begin
          InflowCbx.InValidationError := TRUE;
          InflowCbx.ValidationError := FErrorMessage;
          InflowCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateOutflowDownstreamNode;
const OPNAME = 'TWetlandValidator.ValidateOutflowDownstreamNode';
var
  lWetland  : IWetland;
begin
  try
    lWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (lWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        FErrorMessage := '';
        if (lWetland.Validate(FErrorMessage, 'ReturnFlowDownstreamNode')) then
        begin
          OutflowCbx.InValidationError := FALSE;
          OutflowCbx.ShowErrorState(FALSE);
        end
        else
        begin
          OutflowCbx.InValidationError := TRUE;
          OutflowCbx.ValidationError := FErrorMessage;
          OutflowCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.RePopulateNodes;
const OPNAME = 'TWetlandValidator.RePopulateNodes';
var
  lReservoirList    : IReservoirDataList;
  lIndexA           : integer;
  lReservoir        : IReservoirConfigurationData;
  lWetland          : IWetland;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
      if (lWetland <> nil) then
      begin
        with WetlandDialog do
        begin
          OutflowCbx.Clear;
          InflowCbx.Clear;
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
          begin
            for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].ReservoirConfigurationData;
              if (lReservoir.ReservoirIdentifier = 0) then
              begin
                if (lWetland.OutflowChannel <> nil) then
                  OutflowCbx.Items.AddObject('(0) ' +
                  UpperCase(lWetland.OutflowChannel.SinkName), TObject(lReservoir.ReservoirIdentifier));
                if (lWetland.InflowChannel <> nil) then
                  InflowCbx.Items.AddObject('(0) ' +
                  UpperCase(lWetland.InflowChannel.SourceName), TObject(lReservoir.ReservoirIdentifier));
              end
              else
              begin
                if (lReservoir.NodeType <> ntIrrigationNode) then
                begin
                  OutflowCbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoir.ReservoirIdentifier));
                  InflowCbx.Items.AddObject
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

procedure TWetlandValidator.SetGridGraphBtnState(AReservoir: IReservoirData);
const OPNAME = 'TWetlandValidator.SetGridGraphBtnState';
var
  LFileNamesList : TFileNamesList;
  LFileNameObject: TAbstractModelFileName;
  LIndex         : integer;
begin
  try
    WetlandDialog.DamLevelsFileNameGridBtn.Enabled := False;
    WetlandDialog.DamLevelsFileNameGraphBtn.Enabled := False;
    if(AReservoir <> nil) and (AReservoir.ReservoirConfigurationData.DamLevelsFileName <> '') then
    begin
      LFileNameObject := nil;
      LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDamLevelsFileNames;
      for LIndex:= 0 to LFileNamesList.FilesCount-1 do
      begin
        if(UpperCase(AReservoir.ReservoirConfigurationData.DamLevelsFileName) = UpperCase(LFileNamesList.FileNameObject[LIndex].ShortName)) then
        begin
          LFileNameObject := LFileNamesList.FileNameObject[LIndex];
          Break;
        end;
      end;
      if(LFileNameObject <> nil) then
      begin
        WetlandDialog.DamLevelsFileNameGridBtn.Enabled  := LFileNameObject.SavedInDB;
        WetlandDialog.DamLevelsFileNameGraphBtn.Enabled := LFileNameObject.SavedInDB;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateDamLevelFileName(AReservoir: IReservoirData);
const OPNAME = 'TWetlandValidator.ValidateDamLevelFileName';
begin
  try
    with WetlandDialog do
    begin
      FErrorMessage := '';
      if (AReservoir.ReservoirConfigurationData.Validate(FErrorMessage, 'DamLevelsFileName')) then
      begin
        DamLevelsFileNameCbx.InValidationError := FALSE;
        DamLevelsFileNameCbx.ValidationError := '';
        DamLevelsFileNameCbx.ShowErrorState(FALSE);
      end
      else
      begin
        DamLevelsFileNameCbx.InValidationError := TRUE;
        DamLevelsFileNameCbx.ValidationError := FErrorMessage;
        DamLevelsFileNameCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateRainCoef(AReservoir: IReservoirConfigurationData);
const OPNAME = 'TWetlandValidator.ValidateRainCoef';
begin
  try
    with WetlandDialog do
    begin
      FErrorMessage := '';
      RainCoeffEdit.ContextValidationError := FErrorMessage;
      if (not AReservoir.Validate(FErrorMessage, 'RainCoef')) then
        FAllErrorMessages.Add(FErrorMessage);
      RainCoeffEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateReservoirPenaltyStructure(AReservoir: IReservoirData);
const OPNAME = 'TWetlandValidator.ValidateReservoirPenaltyStructure';
begin
  try
    with WetlandDialog do
    begin
      FErrorMessage := '';
      PenaltyStructureEdit.ContextValidationError := FErrorMessage;
      if (not AReservoir.ReservoirConfigurationData.Validate(FErrorMessage,'ReservoirPenaltyStructure')) then
        FAllErrorMessages.Add(FErrorMessage);
      PenaltyStructureEdit.FieldValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateReservoirPriority(AReservoir: IReservoirData);
const OPNAME = 'TWetlandValidator.ValidateReservoirPriority';
begin
  try
    with WetlandDialog do
    begin
      FErrorMessage := '';
      PriorityEdit.ContextValidationError := FErrorMessage;
      if (not AReservoir.ReservoirConfigurationData.Validate(FErrorMessage, 'ReservoirPriority')) then
        FAllErrorMessages.Add(FErrorMessage);
      PriorityEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.RepopulateHistoricWaterLevels;
const OPNAME = 'TWetlandValidator.RepopulateHistoricWaterLevels';
var
  LFileNamesList  : TFileNamesList;
  LFileName       : string;
  LIndex          : integer;
  lReservoir      : IReservoirData;
  LWetland        : IWetland;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      with WetlandDialog do
      begin
        DamLevelsFileNameCbx.Items.Clear;
        LFileNamesList := TYieldModelDataObject(FAppModules.Model.ModelData).CastFileNamesObject.CastDamLevelsFileNames;
        for LIndex:= 0 to LFileNamesList.FilesCount-1 do
        begin
          LFileName := LFileNamesList.FileNameObject[LIndex].FileName;
          DamLevelsFileNameCbx.Items.Add(LFileName);
        end;
        lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
            NetworkElementData.ReservoirList.ReservoirByIdentifier[LWetland.NodeNumber];
        if (lReservoir <> nil)  and (lReservoir.ReservoirConfigurationData.DamLevelsFileName <> '')then
          DamLevelsFileNameCbx.ItemIndex := DamLevelsFileNameCbx.Items.IndexOf(lReservoir.ReservoirConfigurationData.DamLevelsFileName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWetlandValidator.OnNewPenaltyClick(Sender: TObject);
const OPNAME = 'TWetlandValidator.OnNewPenaltyClick';
begin
  try

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.OnSelectHistoricWaterlevelClick(Sender: TObject);
const OPNAME = 'TWetlandValidator.OnSelectHistoricWaterlevelClick';
var
  LFileSelector : TOpenDialog;
  LFileName     : string;
  lReservoir    : IReservoirData;
  lMessage      : string;
  LWetland      : IWetland;
begin
  try
    LFileSelector := TOpenDialog.Create(nil);
    try
      LFileSelector.Filter  := 'All Files|*.*| (*.TXT)| *.txt|(*.ABS)|*.abs|(*.CIR)|*.cir|(*.DEM)|*.dem|(*.IRR)|*.IRR|(*.IRD)|*.ird|(*.URB)|*.urb';
      LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      if LFileSelector.Execute then
      begin
        LFileName := LFileSelector.FileName;
        if(UpperCase(LFileName) <> UpperCase( WetlandDialog.DamLevelsFileNameCbx.Text)) then
        begin
          LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
          if (LWetland <> nil) then
            lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList.ReservoirByIdentifier[lWetland.NodeNumber];
          if (lReservoir <> nil) then
          begin
            with WetlandDialog do
            begin
              if (FAppModules.FieldProperties.ValidateFieldProperty(
                  DamLevelsFileNameCbx.FieldProperty.FieldName,
                  LFileName,lMessage)) then
              begin
                lReservoir.ReservoirConfigurationData.DamLevelsFileName := LFileName;
                RepopulateHistoricWaterLevels;
                DamLevelsFileNameCbx.ItemIndex := DamLevelsFileNameCbx.Items.IndexOf(lReservoir.ReservoirConfigurationData.DamLevelsFileName);
                DoContextValidation(dvtDamlevelFileName);
                SetGridGraphBtnState(lReservoir);
              end
              else
                DamLevelsFileNameCbx.ValidationError := lMessage;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LFileSelector);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.OnSelectPenaltyClick(Sender: TObject);
const OPNAME = 'TWetlandValidator.OnSelectPenaltyClick';
var
  LForm             : TYieldModelDataGUIForm;
  LDialogValidator  : TReservoirPenaltyValidator;
  FSelectedPenalty  : Integer;
  LReservoirObject  : IReservoirData;
  LWetland          : IWetland;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
    begin
      LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
      try
        LForm.Initialise;
        LForm.LanguageHasChanged;

        LDialogValidator := TReservoirPenaltyValidator.Create(LForm,FAppModules);
        try

          LForm.AddModelDataPanel(LDialogValidator.Panel);
          LDialogValidator.Initialise;
          LReservoirObject :=
            TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[LWetland.NodeNumber];
          if (LReservoirObject <> nil) then
            LDialogValidator.PenaltyStructureNumber := LReservoirObject.ReservoirConfigurationData.PenaltyStructIdentifier;
          LDialogValidator.ViewMode := vmSelect;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LForm.ShowModal;
          if (LForm.ModalResult = mrOk) then
          begin
            FSelectedPenalty := TReservoirPenaltyDialog(LDialogValidator.Panel).GrdPenalty.Col -3;
            if(WetlandDialog.PenaltyStructureEdit.Text <> IntToStr(FSelectedPenalty)) then
            begin
              WetlandDialog.PenaltyStructureEdit.Text := IntToStr(FSelectedPenalty);
              WetlandDialog.PenaltyStructureEdit.OnExit(WetlandDialog.PenaltyStructureEdit);
              DoContextValidation(dvtReservoirPenalty);
            end;
          end;
        finally
          LDialogValidator.Free;
        end;
      finally
        LForm.Free;
      end;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.OnViewGraphClick(Sender: TObject);
const OPNAME = 'TWetlandValidator.OnViewGraphClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRAPH');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      LContextDataList.Add('MODELELEMENTTYPE=DAMLEVELS');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.OnReservoirExistsClick(Sender: TObject);
const OPNAME = 'TWetlandValidator.OnReservoirExistsClick';
begin
  try
    if(WetlandDialog.ReservoirExistsChkBox.HasValueChanged) then
      UpdateReservoirExists;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.OnSummaryIncludeClick(Sender: TObject);
const OPNAME = 'TWetlandValidator.OnSummaryIncludeClick';
begin
  try
    if(WetlandDialog.SummaryIncludeChkBox.HasValueChanged) then
      UpdateResevoirIncludeSummary;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.OnViewGridClick(Sender: TObject);
const OPNAME = 'TWetlandValidator.OnViewGridClick';
var
  LContextDataList: TStringList;
begin
  try
    LContextDataList := TStringList.Create;
    try
      LContextDataList.Add('VIEWNAME=INPUTVIEWDATAGRID');
      LContextDataList.Add(Format('MODELELEMENTID=%d',[FIdentifier]));
      LContextDataList.Add('MODELELEMENTTYPE=DAMLEVELS');
      FAppModules.Model.ViewInputPopupDialog(nil, LContextDataList.CommaText, nil);
    finally
      LContextDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateDamLevelFileName;
const OPNAME = 'TWetlandValidator.UpdateDamLevelFileName';
var
  lReservoir : IReservoirData;
  lMessage   : string;
  LWetland   : IWetland;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
      lReservoir := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ReservoirList.ReservoirByIdentifier[lWetland.NodeNumber];
    if (lReservoir <> nil) then
    begin
      with WetlandDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DamLevelsFileNameCbx.FieldProperty.FieldName,
            DamLevelsFileNameCbx.Text,lMessage)) then
        begin
          lReservoir.ReservoirConfigurationData.DamLevelsFileName := DamLevelsFileNameCbx.Text;
          DamLevelsFileNameCbx.SetFieldIndex(DamLevelsFileNameCbx.Items.IndexOf(lReservoir.ReservoirConfigurationData.DamLevelsFileName));
          DoContextValidation(dvtDamlevelFileName);
        end
        else
          DamLevelsFileNameCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateRainCoef;
const OPNAME = 'TWetlandValidator.UpdateRainCoef';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LWetland         : IWetland;
begin
  try
    if WetlandDialog.RainCoeffEdit.HasValueChanged then
    begin
      LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
      if (LWetland <> nil) then
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[lWetland.NodeNumber];
      if (LReservoirObject <> nil) then
      begin
        with WetlandDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty('RainCoef',
             RainCoeffEdit.Text, lErrorMessage) then
          begin
            RainCoeffEdit.FieldValidationError := lErrorMessage;
            LReservoirObject.ReservoirConfigurationData.RainCoef := StrToFloat(RainCoeffEdit.Text);
            RainCoeffEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.RainCoef);
            DoContextValidation(dvtResPropRainCoef);
          end
          else
            RainCoeffEdit.FieldValidationError := lErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateReservoirExists;
const OPNAME = 'TWetlandValidator.UpdateReservoirExists';
var
  LReservoirObject: IReservoirData;
  LReservoirExists : integer;
  LErrorMessage: string;
  LWetland  : IWetland;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[lWetland.NodeNumber];
    if (LReservoirObject <> nil) then
    begin
      LReservoirExists := LReservoirObject.ReservoirConfigurationData.StatusIndicator;

      if((LReservoirExists = 1) and ( not WetlandDialog.ReservoirExistsChkBox.Checked) or
         (LReservoirExists <> 1) and (WetlandDialog.ReservoirExistsChkBox.Checked)) then
      begin
        if WetlandDialog.ReservoirExistsChkBox.Checked then
          LReservoirExists := 1
        else
          LReservoirExists := 0;

      if FAppModules.FieldProperties.ValidateFieldProperty(
         WetlandDialog.ReservoirExistsChkBox.FieldProperty.FieldName,
         IntToStr(LReservoirExists),LErrorMessage) then
        begin
          LReservoirObject.ReservoirConfigurationData.StatusIndicator := LReservoirExists;
          WetlandDialog.ReservoirExistsChkBox.Checked     :=
            (LReservoirObject.ReservoirConfigurationData.StatusIndicator = 1);
        end
        else
        begin
          WetlandDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateResevoirIncludeSummary;
const OPNAME = 'TWetlandValidator.UpdateResevoirIncludeSummary';
var
  LReservoirObject: IReservoirData;
  LIncludeSummary : string;
  LErrorMessage: string;
  LWetland : IWetland;
begin
  try
    LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
    if (LWetland <> nil) then
      LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[lWetland.NodeNumber];
    if (LReservoirObject <> nil) then
    begin
      LIncludeSummary := UpperCase(Trim(LReservoirObject.ReservoirConfigurationData.IncludeSummary));

      if((LIncludeSummary = 'Y') and ( not WetlandDialog.SummaryIncludeChkBox.Checked) or
         (LIncludeSummary <> 'Y') and (WetlandDialog.SummaryIncludeChkBox.Checked)) then
      begin
        if WetlandDialog.SummaryIncludeChkBox.Checked then
          LIncludeSummary := 'Y'
        else
          LIncludeSummary := 'N';

        if FAppModules.FieldProperties.ValidateFieldProperty(
         WetlandDialog.SummaryIncludeChkBox.FieldProperty.FieldName,
         LIncludeSummary,LErrorMessage) then
        begin
          LReservoirObject.ReservoirConfigurationData.IncludeSummary := LIncludeSummary;
          WetlandDialog.SummaryIncludeChkBox.Checked     :=
            (LReservoirObject.ReservoirConfigurationData.IncludeSummary = 'Y');
        end
        else
        begin
          WetlandDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateResevoirPenaltyStructure;
const OPNAME = 'TWetlandValidator.UpdateResevoirPenaltyStructure';
var
  LReservoirObject      : IReservoirData;
  LPenaltyStructureList : IReservoirPenaltyList;
  LPenaltyCountsData    : IReservoirPenaltyCounts;
  LErrorMessage         : string;
  LWetland              : IWetland;
begin
  try
    if(WetlandDialog.PenaltyStructureEdit.HasValueChanged) then
    begin
      LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
      if (LWetland <> nil) then
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[lWetland.NodeNumber];
      if (LReservoirObject <> nil) then
      begin
        if FAppModules.FieldProperties.ValidateFieldProperty(
           WetlandDialog.PenaltyStructureEdit.FieldProperty.FieldName,
           WetlandDialog.PenaltyStructureEdit.Text,LErrorMessage) then
        begin
          LPenaltyStructureList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirPenaltyStructureList;
          if (LPenaltyStructureList <> nil) then
          begin
            LPenaltyCountsData := LPenaltyStructureList.ReservoirPenaltyCounts;
            if (LPenaltyCountsData <> nil) then
            begin
              if  (StrToInt(WetlandDialog.PenaltyStructureEdit.Text) >  LPenaltyCountsData.PenaltyStructureCount) then
                WetlandDialog.PenaltyStructureEdit.FieldValidationError := 'invalid penalty structure number'
              else
              begin
                WetlandDialog.PenaltyStructureEdit.FieldValidationError :=  LErrorMessage;
                LReservoirObject.ReservoirConfigurationData.PenaltyStructIdentifier := StrToInt(WetlandDialog.PenaltyStructureEdit.Text);
                WetlandDialog.PenaltyStructureEdit.SetFieldValue(WetlandDialog.PenaltyStructureEdit.Text);
                DoContextValidation(dvtReservoirPenalty);
              end;
            end;
          end;
        end
        else
        begin
          WetlandDialog.PenaltyStructureEdit.FieldValidationError :=  LErrorMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateResevoirPriority;
const OPNAME = 'TWetlandValidator.UpdateResevoirPriority';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LWetland         : IWetland;
begin
  try
    if WetlandDialog.PriorityEdit.HasValueChanged then
    begin
      LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
      if (LWetland <> nil) then
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[lWetland.NodeNumber];
      if (LReservoirObject <> nil) then
      begin
        with WetlandDialog do
        begin
          if FAppModules.FieldProperties.ValidateFieldProperty(
           'ReservoirPriority',PriorityEdit.Text, lErrorMessage) then
          begin
            PriorityEdit.FieldValidationError := LErrorMessage;
            LReservoirObject.ReservoirConfigurationData.Priority := StrToFloat(PriorityEdit.Text);
            PriorityEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.Priority);
            DoContextValidation(dvtResPropPriority);
          end
          else
            PriorityEdit.FieldValidationError := lErrorMessage;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateXCoord;
const OPNAME = 'TWetlandValidator.UpdateXCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LWetland         : IWetland;
begin
  try
    if WetlandDialog.WetlandXCoordEdit.HasValueChanged then
    begin
      LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
      if (LWetland <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lWetland.NodeNumber];
        if (LReservoirObject <> nil) then
        begin
          with WetlandDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('XCoord',WetlandXCoordEdit.Text, lErrorMessage) then
            begin
              WetlandXCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.XCoord := StrToFloat(WetlandXCoordEdit.Text);
              WetlandXCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.XCoord);
            end
            else
              WetlandXCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.UpdateYCoord;
const OPNAME = 'TWetlandValidator.UpdateYCoord';
var
  lErrorMessage    : string;
  LReservoirObject : IReservoirData;
  LWetland         : IWetland;
begin
  try
    if WetlandDialog.WetlandYCoordEdit.HasValueChanged then
    begin
      LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.WetlandByID[FIdentifier];
      if (LWetland <> nil) then
      begin
        LReservoirObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lWetland.NodeNumber];
        if (LReservoirObject <> nil) then
        begin
          with WetlandDialog do
          begin
            if FAppModules.FieldProperties.ValidateFieldProperty('YCoord',WetlandYCoordEdit.Text, lErrorMessage) then
            begin
              WetlandYCoordEdit.FieldValidationError := lErrorMessage;
              LReservoirObject.ReservoirConfigurationData.YCoord := StrToFloat(WetlandYCoordEdit.Text);
              WetlandYCoordEdit.SetFieldValue(LReservoirObject.ReservoirConfigurationData.YCoord);
            end
            else
              WetlandYCoordEdit.FieldValidationError := lErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateXCoord(AReservoirData: IReservoirData);
const OPNAME = 'TWetlandValidator.ValidateXCoord';
begin
  try
    with WetlandDialog do
    begin
      FErrorMessage := '';
      WetlandXCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'XCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      WetlandXCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWetlandValidator.ValidateYCoord(AReservoirData: IReservoirData);
const OPNAME = 'TWetlandValidator.ValidateYCoord';
begin
  try
    with WetlandDialog do
    begin
      FErrorMessage := '';
      WetlandYCoordEdit.ContextValidationError := FErrorMessage;
      if (NOT AReservoirData.ReservoirConfigurationData.Validate(FErrorMessage, 'YCoord')) then
        FAllErrorMessages.Add(FErrorMessage);
      WetlandYCoordEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

