{******************************************************************************}
{*  UNIT      : Contains the class TPowerPlantValidator.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UPowerPlantValidator;

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
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UPowerPlantDialog;

type
  TPowerPlantValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnPlantExistsClick(Sender: TObject);
    procedure OnDownstreamPlantsClick(Sender: TObject);
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure RePopulateDataViewer;
    procedure RePopulateNodes;
    procedure RePopulateDownstreamPowerChannels;
    procedure CheckDownstreamPowerChannels;
    procedure UpdateFeatureName;
    procedure UpdatePowerChannelUpstreamNode;
    procedure UpdatePowerChannelDownstreamNode;
    procedure UpdateSpillChannelDownstreamNode;
    procedure UpdateMaxGeneratorCapacity;
    procedure UpdateMaxTurbineCapacity;
    procedure UpdatePlantExists;
    procedure UpdateHeadLoss;
    procedure UpdateDownstreamPlants;
    procedure ValidateFeatureName (AFeature : IPowerPlant);
    procedure ValidatePowerSpillUpstreamNode (AFeature : IPowerPlant);
    procedure ValidatePowerDownstreamNode (AFeature : IPowerPlant);
    procedure ValidateSpillDownstreamNode (AFeature : IPowerPlant);
    procedure ValidateMaximumGeneratorCapacity (AFeature : IPowerPlant);
    procedure ValidateMaximumTurbineCapacity (AFeature : IPowerPlant);
    procedure ValidateHeadLoss (AFeature : IPowerPlant);
    procedure ValidateDownstreamPowerPlants (AFeature : IPowerPlant);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function PowerPlantDialog : TPowerPlantDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  Contnrs,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UNetworkElementData,
  UNetworkFeaturesData;

{******************************************************************************}
{* TPowerPlantValidator                                                       *}
{******************************************************************************}

procedure TPowerPlantValidator.CreateMemberObjects;
const OPNAME = 'TPowerPlantValidator.CreateMemberObjects';
var
  lPanel : TPowerPlantDialog;
begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TPowerPlantDialog.Create(FPanelOwner,FAppModules);
    lPanel := PowerPlantDialog;
    with lPanel do
    begin
      FeatureNameEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('PowerPlantName');
      FeatureNameEdit.OnEnter        := OnEditControlEnter;
      FeatureNameEdit.OnExit         := OnEditControltExit;

      MaxGeneratorEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('MaxCapGenerator');
      MaxGeneratorEdit.OnEnter       := OnEditControlEnter;
      MaxGeneratorEdit.OnExit        := OnEditControltExit;

      UpStreamNodeCbx.FieldProperty  := FAppModules.FieldProperties.FieldProperty('PowerChannelUpstreamNode');
      UpStreamNodeCbx.OnEnter        := OnEditControlEnter;
      UpStreamNodeCbx.OnExit         := OnEditControltExit;

      PowerDownStreamNodeCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('PowerChannelDownstreamNode');
      PowerDownStreamNodeCbx.OnEnter       := OnEditControlEnter;
      PowerDownStreamNodeCbx.OnExit        := OnEditControltExit;

      SpillDownStreamNodeCbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('SpillChannelDownstreamNode');
      SpillDownStreamNodeCbx.OnEnter       := OnEditControlEnter;
      SpillDownStreamNodeCbx.OnExit        := OnEditControltExit;

      MaxTurbineEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('MaxCapTurbine');
      MaxTurbineEdit.OnEnter         := OnEditControlEnter;
      MaxTurbineEdit.OnExit          := OnEditControltExit;

      PlantExistsChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('PowerPlantStatus');
      PlantExistsChkBox.OnEnter       := OnEditControlEnter;
      PlantExistsChkBox.OnClick       := OnPlantExistsClick;

      HeadLossEdit.FieldProperty   := FAppModules.FieldProperties.FieldProperty('HeadLoss');
      HeadLossEdit.OnEnter         := OnEditControlEnter;
      HeadLossEdit.OnExit          := OnEditControltExit;

      DowstreamPlantsCheckLbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('DownStreamPowerChannelNumber');
      DowstreamPlantsCheckLbx.OnEnter       := OnEditControlEnter;
      DowstreamPlantsCheckLbx.OnClick       := OnDownstreamPlantsClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.DestroyMemberObjects;
const OPNAME = 'TPowerPlantValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantValidator.Initialise: boolean;
const OPNAME = 'TPowerPlantValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantValidator.LanguageHasChanged: boolean;
const OPNAME = 'TPowerPlantValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.PowerPlant');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.ClearDataViewer;
const OPNAME = 'TPowerPlantValidator.ClearDataViewer';
var
  lPanel : TPowerPlantDialog;
begin
  inherited ClearDataViewer;
  try
    lPanel := PowerPlantDialog;
    with lPanel do
    begin
      FeatureNameEdit.SetFieldValue('');
      MaxGeneratorEdit.Text := '-1.0';
      MaxTurbineEdit.Text   := '-1.0';
      PlantExistsChkBox.Checked := FALSE;
      HeadLossEdit.Text     := '-1.0';
      DowstreamPlantsCheckLbx.Items.Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.PopulateDataViewer;
const OPNAME = 'TPowerPlantValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtPowerPlantInfo);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.RePopulateNodes;
const OPNAME = 'TPowerPlantValidator.RePopulateNodes';
var
  lReservoirList  : IReservoirDataList;
  lIndexA         : integer;
  lReservoir      : IReservoirConfigurationData;
  lPowerPlant     : IPowerPlant;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lPowerPlant <> nil) then
      begin
        with PowerPlantDialog do
        begin
          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
          begin
            for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].ReservoirConfigurationData;
              if (lReservoir.ReservoirIdentifier = 0) then
              begin
                if (lPowerPlant.PowerChannel <> nil) then
                begin
                  UpStreamNodeCbx.Items.AddObject('(0) ' +
                    UpperCase(lPowerPlant.PowerChannel.SourceName), TObject(lReservoir.ReservoirIdentifier));
                  PowerDownStreamNodeCbx.Items.AddObject('(0) ' +
                    UpperCase(lPowerPlant.PowerChannel.SinkName), TObject(lReservoir.ReservoirIdentifier));
                end;
                if (lPowerPlant.SpillChannel <> nil) then
                  SpillDownStreamNodeCbx.Items.AddObject('(0) ' +
                    UpperCase(lPowerPlant.SpillChannel.SinkName), TObject(lReservoir.ReservoirIdentifier));
              end
              else
              begin
                if (lReservoir.NodeType = ntReservoir) then
                  UpStreamNodeCbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoir.ReservoirIdentifier));
                if (lReservoir.NodeType <> ntIrrigationNode) then
                begin
                  PowerDownStreamNodeCbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoir.ReservoirIdentifier));
                  SpillDownStreamNodeCbx.Items.AddObject
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

procedure TPowerPlantValidator.RePopulateDataViewer;
const OPNAME = 'TPowerPlantValidator.RePopulateDataViewer';
var
  lPowerPlant    : IPowerPlant;
  lReservoirData : IReservoirData;
  lReservoir     : IReservoirConfigurationData;
  lFieldProperty : TAbstractFieldProperty;
  lFieldIndex    : string;
  lKeyValues     : string;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lPowerPlant <> nil) then
      begin
        with PowerPlantDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := FeatureNameEdit.FieldProperty;
          lKeyValues     := lPowerPlant.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          FeatureNameEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          FeatureNameEdit.SetFieldValue(lPowerPlant.FeatureName);

          RePopulateNodes;
          lFieldProperty := UpStreamNodeCbx.FieldProperty;
          UpStreamNodeCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          lFieldProperty := PowerDownStreamNodeCbx.FieldProperty;
          PowerDownStreamNodeCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          if (lPowerPlant.PowerChannel <> nil) then
          begin
            lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lPowerPlant.PowerChannel.UpStreamNodeNumber];
            if (lReservoirData <> nil) then
            begin
              lReservoir := lReservoirData.ReservoirConfigurationData;
              UpstreamNodeCbx.SetFieldIndex
                (UpstreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
            end;
            lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lPowerPlant.PowerChannel.DownStreamNodeNumber];
            if (lReservoirData <> nil) then
            begin
              lReservoir := lReservoirData.ReservoirConfigurationData;
              PowerDownStreamNodeCbx.SetFieldIndex
                (PowerDownStreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
            end;
          end;
          lFieldProperty := SpillDownStreamNodeCbx.FieldProperty;
          SpillDownStreamNodeCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          if (lPowerPlant.SpillChannel <> nil) then
          begin
            lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[lPowerPlant.SpillChannel.DownStreamNodeNumber];
            if (lReservoirData <> nil) then
            begin
              lReservoir := lReservoirData.ReservoirConfigurationData;
              SpillDownStreamNodeCbx.SetFieldIndex
                (SpillDownStreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
            end;
          end;

          lFieldProperty := MaxGeneratorEdit.FieldProperty;
          MaxGeneratorEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          MaxGeneratorEdit.SetFieldValue(lPowerPlant.MaximumGeneratorCapacity);

          lFieldProperty := MaxTurbineEdit.FieldProperty;
          MaxTurbineEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          MaxTurbineEdit.SetFieldValue(lPowerPlant.MaximumTurbineCapacity);

          lFieldProperty := PlantExistsChkBox.FieldProperty;
          PlantExistsChkBox.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          PlantExistsChkBox.Checked := lPowerPlant.PowerPlantStatus;

          lFieldProperty := HeadLossEdit.FieldProperty;
          HeadLossEdit.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          HeadLossEdit.SetFieldValue(lPowerPlant.HeadLoss);

          lFieldProperty := DowstreamPlantsCheckLbx.FieldProperty;
          DowstreamPlantsCheckLbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
          RePopulateDownstreamPowerChannels;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.RePopulateDownstreamPowerChannels;
const OPNAME = 'TPowerPlantValidator.RePopulateDownstreamPowerChannels';
var
  lIndex       : integer;
  lChannelList : IChannelList;
  lChannel     : IGeneralFlowChannel;
  lPowerPlant  : IPowerPlant;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lPowerPlant <> nil) then
      begin
        lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList;
        if (lChannelList <> nil) then
        begin
          with PowerPlantDialog do
          begin
            DowstreamPlantsCheckLbx.Items.Clear;
            for lIndex := 0 to lChannelList.ChannelCount - 1 do
            begin
              lChannel := lChannelList.ChannelByIndex[lIndex];
              if ((lChannel.ChannelType = 3) AND
                  (LChannel.ChannelSubType = 1) AND
                  (lPowerPlant.PowerChannel.ChannelNumber <> lChannel.ChannelNumber)) then
              begin
                DowstreamPlantsCheckLbx.Items.AddObject
                  ('(' + IntToStr(lChannel.ChannelNumber) + ') ' + lChannel.ChannelName,
                   TObject(lChannel.ChannelNumber));
              end;
            end;
          end;
          CheckDownstreamPowerChannels;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.CheckDownstreamPowerChannels;
const OPNAME = 'TPowerPlantValidator.CheckDownstreamPowerChannels';
var
  lIndex       : integer;
  lCount       : integer;
  lFound       : boolean;
  lChannelNr   : integer;
  lPowerPlant  : IPowerPlant;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lPowerPlant <> nil) then
      begin
        with PowerPlantDialog do
        begin
          for lIndex := 0 to lPowerPlant.DownstreamPowerChannelNrsCount - 1 do
          begin
            lChannelNr := lPowerPlant.DownstreamPowerChannelNrByIndex[lIndex];
            lFound := FALSE;
            lCount := 0;
            while ((NOT lFound) AND (lCount < DowstreamPlantsCheckLbx.Items.Count)) do
            begin
              if (lChannelNr = Integer(DowstreamPlantsCheckLbx.Items.Objects[lCount])) then
                lFound := TRUE
              else
                lCount := lCount + 1;
            end;
            if (lFound) then
              DowstreamPlantsCheckLbx.Checked[lCount] := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantValidator.SaveState: boolean;
const OPNAME = 'TPowerPlantValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantValidator.PowerPlantDialog : TPowerPlantDialog;
const OPNAME = 'TPowerPlantValidator.PowerPlantDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TPowerPlantDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TPowerPlantValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantValidator.StudyHasChanged: boolean;
const OPNAME = 'TPowerPlantValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TPowerPlantValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TPowerPlantValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with PowerPlantDialog do
    begin
      if ((Sender = FeatureNameEdit) AND
          (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName
      else
      if ((Sender = UpStreamNodeCbx) AND
          (UpStreamNodeCbx.HasValueChanged)) then
        UpdatePowerChannelUpstreamNode
      else
      if ((Sender = PowerDownStreamNodeCbx) AND
          (PowerDownStreamNodeCbx.HasValueChanged)) then
        UpdatePowerChannelDownstreamNode
      else
      if ((Sender = SpillDownStreamNodeCbx) AND
          (SpillDownStreamNodeCbx.HasValueChanged)) then
        UpdateSpillChannelDownstreamNode
      else
      if ((Sender = MaxGeneratorEdit) AND
          (MaxGeneratorEdit.HasValueChanged)) then
        UpdateMaxGeneratorCapacity
      else
      if ((Sender = MaxTurbineEdit) AND
          (MaxTurbineEdit.HasValueChanged)) then
        UpdateMaxTurbineCapacity
      else
      if ((Sender = HeadLossEdit) AND
          (HeadLossEdit.HasValueChanged)) then
        UpdateHeadLoss;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.OnPlantExistsClick(Sender: TObject);
const OPNAME = 'TPowerPlantValidator.OnPlantExistsClick';
begin
  try
    if(PowerPlantDialog.PlantExistsChkBox.HasValueChanged) then
      UpdatePlantExists;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.OnDownstreamPlantsClick(Sender: TObject);
const OPNAME = 'TPowerPlantValidator.OnDownstreamPlantsClick';
begin
  try
    if(PowerPlantDialog.DowstreamPlantsCheckLbx.HasValueChanged) then
      UpdateDownstreamPlants;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.UpdateFeatureName;
const OPNAME = 'TPowerPlantValidator.UpdateFeatureName';
var
  lPowerPlant : IPowerPlant;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil) then
    begin
      with PowerPlantDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            FeatureNameEdit.FieldProperty.FieldName,
            FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lPowerPlant.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lPowerPlant.FeatureName);
          DoContextValidation(dvtPowerPlantName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.UpdatePowerChannelUpstreamNode;
const OPNAME = 'TPowerPlantValidator.UpdatePowerChannelUpstreamNode';
var
  lReservoir     : IReservoirConfigurationData;
  lReservoirNr   : integer;
  lPowerPlant    : IPowerPlant;
  lMessage       : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil) then
    begin
      with PowerPlantDialog do
      begin
        lReservoir   := nil;
        lReservoirNr := -1;
        if (UpStreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(UpstreamNodeCbx.Items.Objects[UpstreamNodeCbx.ItemIndex]);
          lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirByIdentifier[lReservoirNr].ReservoirConfigurationData;
        end;
        if (lReservoir <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'PowerChannelUpstreamNode',IntToStr(lReservoirNr), lMessage)) then
          begin
            lPowerPlant.PowerChannel.UpStreamNodeNumber := lReservoirNr;
            lPowerPlant.SpillChannel.UpStreamNodeNumber := lReservoirNr;
            lReservoirNr := lPowerPlant.PowerChannel.UpStreamNode.ReservoirConfigurationData.ReservoirIdentifier;
            UpstreamNodeCbx.SetFieldIndex(UpstreamNodeCbx.Items.IndexOfObject(TObject(lReservoirNr)));
            DoContextValidation(dvtPowerPlantInfoPowerSpillUpstreamNode);
          end
          else
            UpStreamNodeCbx.ValidationError := lMessage;
        end;    
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.UpdatePowerChannelDownstreamNode;
const OPNAME = 'TPowerPlantValidator.UpdatePowerChannelDownstreamNode';
var
  lReservoir     : IReservoirConfigurationData;
  lReservoirNr   : integer;
  lPowerPlant    : IPowerPlant;
  lMessage       : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil) then
    begin
      with PowerPlantDialog do
      begin
        lReservoir   := nil;
        lReservoirNr := -1;
        if (PowerDownStreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(PowerDownStreamNodeCbx.Items.Objects[PowerDownStreamNodeCbx.ItemIndex]);
          lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
        end;
        if (lReservoir <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'PowerChannelDownstreamNode',IntToStr(lReservoirNr), lMessage)) then
          begin
            lPowerPlant.PowerChannel.DownStreamNodeNumber := lReservoirNr;
            lReservoirNr := lPowerPlant.PowerChannel.DownStreamNode.ReservoirConfigurationData.ReservoirIdentifier;
            PowerDownStreamNodeCbx.SetFieldIndex(PowerDownStreamNodeCbx.Items.IndexOfObject(TObject(lReservoirNr)));
            DoContextValidation(dvtPowerPlantInfoPowerDownstreamNode);
          end
          else
            PowerDownStreamNodeCbx.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.UpdateSpillChannelDownstreamNode;
const OPNAME = 'TPowerPlantValidator.UpdateSpillChannelDownstreamNode';
var
  lReservoir     : IReservoirConfigurationData;
  lReservoirNr   : integer;
  lPowerPlant    : IPowerPlant;
  lMessage       : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil) then
    begin
      with PowerPlantDialog do
      begin
        lReservoir   := nil;
        lReservoirNr := -1;
        if (SpillDownStreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(SpillDownStreamNodeCbx.Items.Objects[SpillDownStreamNodeCbx.ItemIndex]);
          lReservoir   := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr].ReservoirConfigurationData;
        end;
        if (lReservoir <> nil) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'SpillChannelDownstreamNode',IntToStr(lReservoirNr), lMessage)) then
          begin
            lPowerPlant.SpillChannel.DownStreamNodeNumber := lReservoirNr;
            lReservoirNr := lPowerPlant.SpillChannel.DownStreamNode.ReservoirConfigurationData.ReservoirIdentifier;
            SpillDownStreamNodeCbx.SetFieldIndex(SpillDownStreamNodeCbx.Items.IndexOfObject(TObject(lReservoirNr)));
            DoContextValidation(dvtPowerPlantInfoSpillDownstreamNode);
          end
          else
            SpillDownStreamNodeCbx.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.UpdateMaxGeneratorCapacity;
const OPNAME = 'TPowerPlantValidator.UpdateMaxGeneratorCapacity';
var
  lPowerPlant : IPowerPlant;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            MaxGeneratorEdit.FieldProperty.FieldName,
            MaxGeneratorEdit.Text,lMessage)) then
        begin
          MaxGeneratorEdit.FieldValidationError := lMessage;
          lPowerPlant.MaximumGeneratorCapacity := StrToFloat(Trim(MaxGeneratorEdit.Text));
          MaxGeneratorEdit.SetFieldValue(lPowerPlant.MaximumGeneratorCapacity);
          DoContextValidation(dvtPowerPlantMaximumGeneratorCapacity);
        end
        else
          MaxGeneratorEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.UpdateMaxTurbineCapacity;
const OPNAME = 'TPowerPlantValidator.UpdateMaxTurbineCapacity';
var
  lPowerPlant : IPowerPlant;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            MaxTurbineEdit.FieldProperty.FieldName,
            MaxTurbineEdit.Text,lMessage)) then
        begin
          MaxTurbineEdit.FieldValidationError := lMessage;
          lPowerPlant.MaximumTurbineCapacity := StrToFloat(Trim(MaxTurbineEdit.Text));
          MaxTurbineEdit.SetFieldValue(lPowerPlant.MaximumTurbineCapacity);
          DoContextValidation(dvtPowerPlantMaximumTurbineCapacity);
        end
        else
          MaxTurbineEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.UpdatePlantExists;
const OPNAME = 'TPowerPlantValidator.UpdatePlantExists';
var
  lPowerPlant : IPowerPlant;
  lOldStatus  : Boolean;
  lNewStatus  : Boolean;
  lStatus     : string;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil) then
    begin
      with PowerPlantDialog do
      begin
        lOldStatus := lPowerPlant.PowerPlantStatus;
        lNewStatus := PlantExistsChkBox.Checked;
        if (lOldStatus  <> lNewStatus) then
        begin
          if (lNewStatus) then
            lStatus:= '1'
          else
            lStatus := '0';
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              PlantExistsChkBox.FieldProperty.FieldName,
              lStatus,lMessage)) then
          begin
            lPowerPlant.PowerPlantStatus := PlantExistsChkBox.Checked;
            PlantExistsChkBox.Checked := lPowerPlant.PowerPlantStatus;
          end
          else
            PlantExistsChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.UpdateHeadLoss;
const OPNAME = 'TPowerPlantValidator.UpdateHeadLoss';
var
  lPowerPlant : IPowerPlant;
  lMessage    : string;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    if (lPowerPlant <> nil)then
    begin
      with PowerPlantDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            HeadLossEdit.FieldProperty.FieldName,
            HeadLossEdit.Text,lMessage)) then
        begin
          HeadLossEdit.FieldValidationError := lMessage;
          lPowerPlant.HeadLoss := StrToFloat(Trim(HeadLossEdit.Text));
          HeadLossEdit.SetFieldValue(lPowerPlant.HeadLoss);
          DoContextValidation(dvtPowerPlantHeadLoss);
        end
        else
          HeadLossEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.UpdateDownstreamPlants;
const OPNAME = 'TPowerPlantValidator.UpdateDownstreamPlants';
var
  lPowerPlant       : IPowerPlant;
  lIndex            : integer;
  lNewChannel       : IGeneralFlowChannel;
  lChannelList      : IChannelList;
  lDownstreamPlants : TStringList;
  lMessage          : string;
  lChannelNr        : integer;
begin
  try
    lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList;
    if (lPowerPlant <> nil) then
    begin
      with PowerPlantDialog do
      begin
        lDownstreamPlants := TStringList.Create;
        lDownstreamPlants.Clear;
        for lIndex := 0 to DowstreamPlantsCheckLbx.Items.Count - 1 do
        begin
          if (DowstreamPlantsCheckLbx.Checked[lIndex]) then
          begin
            lChannelNr  := Integer(DowstreamPlantsCheckLbx.Items.Objects[lIndex]);
            lNewChannel := lChannelList.ChannelByChannelNumber[lChannelNr];
            if (lNewChannel <> nil) then
              lDownstreamPlants.Add(IntToStr(lNewChannel.ChannelNumber));
          end;
        end;

        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DownStreamPowerChannelCount', IntToStr(lDownstreamPlants.Count), lMessage)) then
        begin
          DowstreamPlantsCheckLbx.InValidationError := FALSE;
          DowstreamPlantsCheckLbx.ShowErrorState(FALSE);
          lPowerPlant.DownstreamPowerChannelNrs := lDownstreamPlants.CommaText;
          CheckDownstreamPowerChannels;
          DoContextValidation(dvtPowerPlantDownstreamPowerPlants);
        end
        else
        begin
          DowstreamPlantsCheckLbx.InValidationError := True;
          DowstreamPlantsCheckLbx.ValidationError := lMessage;
          DowstreamPlantsCheckLbx.ShowErrorState(TRUE);
        end;
        FreeAndNil(lDownstreamPlants);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TPowerPlantValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TPowerPlantValidator.DoContextValidation';
var
  lFeature     : IPowerPlant;
  lFeatureList : IPowerPlantList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PowerPlantList;
      lFeature     := lFeatureList.PowerPlantByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtPowerPlantInfo, dvtPowerPlantInfoWizardStep4,
                                dvtPowerPlantName]) then
          ValidateFeatureName(lFeature);
        if (AValidationType in [dvtPowerPlantInfo, dvtPowerPlantInfoWizardStep1,
                                dvtPowerPlantInfoPowerSpillUpstreamNode]) then
          ValidatePowerSpillUpstreamNode(lFeature);
        if (AValidationType in [dvtPowerPlantInfo, dvtPowerPlantInfoWizardStep2,
                                dvtPowerPlantInfoPowerDownstreamNode]) then
          ValidatePowerDownstreamNode(lFeature);
        if (AValidationType in [dvtPowerPlantInfo, dvtPowerPlantInfoWizardStep3,
                                dvtPowerPlantInfoSpillDownstreamNode]) then
          ValidateSpillDownstreamNode(lFeature);
        if (AValidationType in [dvtPowerPlantInfo, dvtPowerPlantInfoWizardStep4,
                                dvtPowerPlantMaximumGeneratorCapacity]) then
          ValidateMaximumGeneratorCapacity(lFeature);
        if (AValidationType in [dvtPowerPlantInfo, dvtPowerPlantInfoWizardStep4,
                                dvtPowerPlantMaximumTurbineCapacity]) then
          ValidateMaximumTurbineCapacity(lFeature);
        if (AValidationType in [dvtPowerPlantInfo, dvtPowerPlantInfoWizardStep4,
                                dvtPowerPlantHeadLoss]) then
          ValidateHeadLoss(lFeature);
        if (AValidationType in [dvtPowerPlantInfo, dvtPowerPlantInfoWizardStep4,
                                dvtPowerPlantDownstreamPowerPlants]) then
          ValidateDownstreamPowerPlants(lFeature);

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TPowerPlantValidator.DetermineWizardStatus';
var
  lFeature     : IPowerPlant;
  lFeatureList : IPowerPlantList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.PowerPlantList;
      lFeature := lFeatureList.PowerPlantByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        case ASequence of
        1 :
          begin
            DoContextValidation(dvtPowerPlantInfoWizardStep1);
            if ((lFeature.PowerChannel <> nil) AND
                (lFeature.PowerChannel.UpStreamNodeNumber <> 0)) then
            begin
              Result := 1;
              if (FAllErrorMessages.Count = 0) then
                Result := 2;
            end;
          end;
        2 :
          begin
            DoContextValidation(dvtPowerPlantInfoWizardStep2);
            if (lFeature.PowerChannel <> nil) then
            begin
              Result := 1;
              if (FAllErrorMessages.Count = 0) then
                Result := 2;
            end;
          end;
        3 :
          begin
            DoContextValidation(dvtPowerPlantInfoWizardStep3);
            if (lFeature.SpillChannel <> nil) then
            begin
              Result := 1;
              if (FAllErrorMessages.Count = 0) then
                Result := 2;
            end;
          end;
        4 :
          begin
            DoContextValidation(dvtPowerPlantInfoWizardStep4);
            if ((lFeature.MaximumGeneratorCapacity > 0) OR
                (lFeature.MaximumTurbineCapacity > 0) OR
                (lFeature.HeadLoss > 0)) then
            begin
              Result := 1;
              if (FAllErrorMessages.Count = 0) then
                Result := 2;
            end;
          end;
        else
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.ValidateFeatureName (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantValidator.ValidateFeatureName';
begin
  try
    with PowerPlantDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'PowerPlantName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.ValidatePowerSpillUpstreamNode (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantValidator.ValidatePowerSpillUpstreamNode';
begin
  try
    with PowerPlantDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'PowerSpillUpstreamNode')) then
      begin
        UpStreamNodeCbx.InValidationError := FALSE;
        UpStreamNodeCbx.ShowErrorState(FALSE);
      end
      else
      begin
        UpStreamNodeCbx.InValidationError := TRUE;
        UpStreamNodeCbx.ValidationError := FErrorMessage;
        UpStreamNodeCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.ValidatePowerDownstreamNode (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantValidator.ValidatePowerDownstreamNode';
begin
  try
    with PowerPlantDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'PowerDownstreamNode')) then
      begin
        PowerDownStreamNodeCbx.InValidationError := FALSE;
        PowerDownStreamNodeCbx.ShowErrorState(FALSE);
      end
      else
      begin
        PowerDownStreamNodeCbx.InValidationError := TRUE;
        PowerDownStreamNodeCbx.ValidationError := FErrorMessage;
        PowerDownStreamNodeCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.ValidateSpillDownstreamNode (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantValidator.ValidateSpillDownstreamNode';
begin
  try
    with PowerPlantDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'SpillDownstreamNode')) then
      begin
        SpillDownStreamNodeCbx.InValidationError := FALSE;
        SpillDownStreamNodeCbx.ShowErrorState(FALSE);
      end
      else
      begin
        SpillDownStreamNodeCbx.InValidationError := TRUE;
        SpillDownStreamNodeCbx.ValidationError := FErrorMessage;
        SpillDownStreamNodeCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.ValidateMaximumGeneratorCapacity (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantValidator.ValidateMaximumGeneratorCapacity';
begin
  try
    with PowerPlantDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'MaxCapGenerator')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      MaxGeneratorEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.ValidateMaximumTurbineCapacity (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantValidator.ValidateMaximumTurbineCapacity';
begin
  try
    with PowerPlantDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'MaxCapTurbine')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      MaxTurbineEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.ValidateHeadLoss (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantValidator.ValidateHeadLoss';
begin
  try
    with PowerPlantDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'HeadLoss')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      HeadLossEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPowerPlantValidator.ValidateDownstreamPowerPlants (AFeature : IPowerPlant);
const OPNAME = 'TPowerPlantValidator.ValidateDownstreamPowerPlants';
begin
  try
    with PowerPlantDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'DownstreamPowerPlants')) then
      begin
        DowstreamPlantsCheckLbx.InValidationError := False;
        DowstreamPlantsCheckLbx.ValidationError := FErrorMessage;
      end
      else
      begin
        DowstreamPlantsCheckLbx.InValidationError := True;
        DowstreamPlantsCheckLbx.ValidationError := FErrorMessage;
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPowerPlantValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TPowerPlantValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lFeature       : IPowerPlant;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.PowerPlantList.PowerPlantByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with PowerPlantDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = UpStreamNodeCbx) then
            lFieldProperty := UpStreamNodeCbx.FieldProperty
          else
          if (FActiveControl = PowerDownStreamNodeCbx) then
            lFieldProperty := PowerDownStreamNodeCbx.FieldProperty
          else
          if (FActiveControl = SpillDownStreamNodeCbx) then
            lFieldProperty := SpillDownStreamNodeCbx.FieldProperty
          else
          if (FActiveControl = MaxGeneratorEdit) then
            lFieldProperty := MaxGeneratorEdit.FieldProperty
          else
          if (FActiveControl = MaxTurbineEdit) then
            lFieldProperty := MaxTurbineEdit.FieldProperty
          else
          if (FActiveControl = PlantExistsChkBox) then
            lFieldProperty := PlantExistsChkBox.FieldProperty
          else
          if (FActiveControl = HeadLossEdit) then
            lFieldProperty := HeadLossEdit.FieldProperty
          else
          if (FActiveControl = DowstreamPlantsCheckLbx) then
            lFieldProperty := DowstreamPlantsCheckLbx.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            FAppModules.MetaData.ShowMetaData
              (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

