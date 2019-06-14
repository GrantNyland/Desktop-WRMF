unit UYieldModelWizards;

interface

uses
  VCL.Controls,
  VoaimsCom_TLB,
  UWizardStep;

type
  TYieldModelWizard = class(TWizard)
  protected
    procedure LoadMasterControlFeatureSteps (ADialogParent : TWinControl;
                                             AChannel      : IGeneralFlowChannel);
    procedure LoadMinimumFlowFeatureSteps (ADialogParent : TWinControl;
                                           AChannel      : IGeneralFlowChannel);
    procedure LoadLossFeatureSteps (ADialogParent : TWinControl;
                                    AChannel      : IGeneralFlowChannel);
    procedure LoadMinMaxFlowFeatureSteps (ADialogParent : TWinControl;
                                          AChannel      : IGeneralFlowChannel);
    procedure LoadPumpingFeatureSteps (ADialogParent : TWinControl;
                                       AChannel      : IGeneralFlowChannel);
    procedure LoadSpecifiedDemandFeatureSteps (ADialogParent : TWinControl;
                                               AChannel      : IGeneralFlowChannel);
    procedure LoadPhysicalFlowConstraintSteps (ADialogParent : TWinControl;
                                               AChannel      : IGeneralFlowChannel);
    procedure LoadIFRFeatureSteps (ADialogParent : TWinControl;
                                   AChannel      : IGeneralFlowChannel);
    procedure LoadDiversionFeatureSteps (ADialogParent : TWinControl;
                                         AChannel      : IGeneralFlowChannel);
    procedure LoadSpecifiedInflowFeatureSteps (ADialogParent : TWinControl;
                                               AChannel      : IGeneralFlowChannel);
    procedure DoRunYieldFinishClick (Sender : TObject);
    procedure DoSetCloseButtonState (Sender : TObject);
    procedure DoSetRunButtonState (Sender : TObject);
  public
    procedure LoadChannelWizard (ADialogParent  : TWinControl;
                                 AChannelNumber : integer);
    procedure LoadNodeWizard (ADialogParent : TWinControl;
                              ANodeNumber   : integer);
    procedure LoadReservoirWizard (ADialogParent    : TWinControl;
                                   AReservoirNumber : integer);
    procedure LoadIrrigationAreaWizard (ADialogParent : TWinControl;
                                        AFeatureID    : integer);
    procedure LoadPowerPlantWizard (ADialogParent : TWinControl;
                                    AFeatureID    : integer);
    procedure LoadYieldRunHistoricWizard (ADialogParent : TWinControl);
    procedure LoadYieldRunStochasticWizard (ADialogParent : TWinControl);
    procedure LoadYieldRunYRCWizard (ADialogParent : TWinControl);
  end;

implementation

uses
  SysUtils,
  VCL.StdCtrls,
  UYieldModelDataObject,
  UNodePropertiesValidator,
  UReservoirPropertiesValidator,
  UReservoirCatchmentProportionsValidator,
  UReservoirPhysicalCharacteristicsValidator,
  UReservoirEvaporationValidator,
  UReservoirZoneElevationsValidator,
  UReservoirPenaltyValidator,
  UGeneralFlowChannelValidator,
  UMasterControlChannelValidator,
  UMinimumFlowChannelValidator,
  ULossChannelValidator,
  UMinMaxChannelValidator,
  UPumpingFeatureValidator,
  USpecifiedDemandChannelValidator,
  UPhysicalFlowConstraintValidator,
  UIFRFeatureValidator,
  UDiversionFeatureValidator,
  USpecifiedInflowDataValidator,
  UIrrigationAreaValidator,
  UPowerPlantValidator,
  UPowerPlantDemandsValidator,
  UPowerPlantFactorsValidator,
  UPowerPlantTailwaterValidator,
  UYieldRunHistoricValidator,
  UYieldRunStochasticValidator,
  UYieldRunYRCValidator,
  UMainMenuEventType,
  UErrorHandlingOperations, UAbstractObject;

{******************************************************************************}
{* TYieldModelWizard                                                          *}
{******************************************************************************}

procedure TYieldModelWizard.LoadChannelWizard (ADialogParent  : TWinControl;
                                               AChannelNumber : integer);
const OPNAME = 'TYieldModelWizard.LoadChannelWizard';
var
  lStep                 : TWizardStep;
  LChannelProperties    : string;
  LUpanddownstream      : string;
  LUpanddownstreamDescr : string;
  LChannelPenalty       : string;
  LChannelPenaltyDescr  : string;
  lGeneralValidator     : TGeneralFlowChannelValidator;
  lChannel              : IGeneralFlowChannel;
begin
  try
    FSetFinishButtonState := DoSetCloseButtonState;
    FFinishButtonCaption  := FAppModules.Language.GetString('ButtonCaption.Close');
    LChannelProperties := FAppModules.Language.GetString('TYieldModelWizard.ChannelProperties');
    lStep := AddStep(1, 0, LChannelProperties, '', TGeneralFlowChannelValidator);
    lGeneralValidator := TGeneralFlowChannelValidator(lStep.Validator);
    with lGeneralValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      Identifier           := AChannelNumber;
      SelectChannelEnabled := FALSE;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
    LUpanddownstream := FAppModules.Language.GetString('TYieldModelWizard.Upanddownstream');
    LUpanddownstreamDescr := FAppModules.Language.GetString('TYieldModelWizard.UpanddownstreamDescr');
    LChannelPenalty := FAppModules.Language.GetString('TYieldModelWizard.ChannelPenalty');
    LChannelPenaltyDescr := FAppModules.Language.GetString('TYieldModelWizard.ChannelPenaltyDescr');
    AddStep(2, 1, LUpanddownstream, LUpanddownstreamDescr, nil, 1);
    AddStep(3, 1, LChannelPenalty, LChannelPenaltyDescr , nil, 2);
    try
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[AChannelNumber];
      if (lChannel <> nil) then
      begin
        if (lChannel.MasterControlFeature <> nil) then
          LoadMasterControlFeatureSteps(ADialogParent, lChannel);
        if (lChannel.MinimumFlowConstraint <> nil) then
          LoadMinimumFlowFeatureSteps(ADialogParent, lChannel);
        if ((lChannel.LossFeature <> nil) AND
            (ILossFeature(lChannel.LossFeature).FeatureSubType = 0)) then
          LoadLossFeatureSteps(ADialogParent, lChannel);
        if (lChannel.MinMaxFlowConstraint <> nil) then
          LoadMinMaxFlowFeatureSteps(ADialogParent, lChannel);
        if (lChannel.PumpingFeature <> nil) then
          LoadPumpingFeatureSteps(ADialogParent, lChannel);
        if (lChannel.SpecifiedDemandFeature <> nil) then
          LoadSpecifiedDemandFeatureSteps(ADialogParent, lChannel);
        if (lChannel.PhysicalFlowConstraint <> nil) then
          LoadPhysicalFlowConstraintSteps(ADialogParent, lChannel);
        if (lChannel.IFRFeature <> nil) then
          LoadIFRFeatureSteps(ADialogParent, lChannel);
        if (((lChannel.LossFeature <> nil) AND
             (ILossFeature(lChannel.LossFeature).FeatureSubType = 1)) OR
            (lChannel.DiversionFeature <> nil)) then
          LoadDiversionFeatureSteps(ADialogParent, lChannel);
        if (lChannel.SpecifiedInflowFeature <> nil) then
          LoadSpecifiedInflowFeatureSteps(ADialogParent, lChannel);
      end;
    finally
      lChannel := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadMasterControlFeatureSteps (ADialogParent : TWinControl;
                                                           AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadMasterControlFeatureSteps';
var
  lStep                     : TWizardStep;
  LMasterControl            : string;
  LTargetdrafts             : string;
  LTargetdraftsDescr        : string;
  LDistributionfactors      : string;
  LDistributionfactorsDescr : string;
  lMasterValidator          : TMasterControlChannelValidator;
begin
  try
    LMasterControl := FAppModules.Language.GetString('TYieldModelWizard.MasterControl');
    lStep := AddStep(4, 0, LMasterControl, '', TMasterControlChannelValidator);
    lMasterValidator := TMasterControlChannelValidator(lStep.Validator);
    with lMasterValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID    := AChannel.MasterControlFeature.FeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
    LTargetdrafts             := FAppModules.Language.GetString('TYieldModelWizard.Targetdrafts');
    LTargetdraftsDescr        := FAppModules.Language.GetString('TYieldModelWizard.TargetdraftsDescr');
    LDistributionfactors      := FAppModules.Language.GetString('TYieldModelWizard.Distributionfactors');
    LDistributionfactorsDescr := FAppModules.Language.GetString('TYieldModelWizard.DistributionfactorsDescr');
    AddStep(5, 4, LTargetdrafts,
            LTargetdraftsDescr, nil, 1);
    AddStep(6, 4, LDistributionfactors,
            LDistributionfactorsDescr, nil, 2);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadMinimumFlowFeatureSteps (ADialogParent : TWinControl;
                                                         AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadMinimumFlowFeatureSteps';
var
  lStep                    : TWizardStep;
  LMinimumFlowFeature      : string;
  LMinimumFlowFeatureDescr : string;
  lMinimumValidator        : TMinimumFlowChannelValidator;
begin
  try
    LMinimumFlowFeature      := FAppModules.Language.GetString('TYieldModelWizard.MinimumFlowFeature');
    LMinimumFlowFeatureDescr := FAppModules.Language.GetString('TYieldModelWizard.MinimumFlowFeatureDescr');
    lStep := AddStep(4, 0, LMinimumFlowFeature, LMinimumFlowFeatureDescr, TMinimumFlowChannelValidator);
    lMinimumValidator := TMinimumFlowChannelValidator(lStep.Validator);
    with lMinimumValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID    := IMinimumFlowConstraint(AChannel.MinimumFlowConstraint).FeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadLossFeatureSteps (ADialogParent : TWinControl;
                                                  AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadLossFeatureSteps';
var
  lStep             : TWizardStep;
  LLossFeature      : string;
  LLossFeatureDescr : string;
  lLossValidator    : TLossChannelValidator;
begin
  try
    LLossFeature      := FAppModules.Language.GetString('TYieldModelWizard.LossFeature');
    LLossFeatureDescr := FAppModules.Language.GetString('TYieldModelWizard.LossFeatureDescr');
    lStep := AddStep(5, 0, LLossFeature, LLossFeatureDescr, TLossChannelValidator);
    lLossValidator := TLossChannelValidator(lStep.Validator);
    with lLossValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID    := ILossFeature(AChannel.LossFeature).FeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadMinMaxFlowFeatureSteps (ADialogParent : TWinControl;
                                                        AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadMinMaxFlowFeatureSteps';
var
  lStep                   : TWizardStep;
  LMinMaxFlowFeature      : string;
  LMinMaxFlowFeatureDescr : string;
  lMinMaxValidator        : TMinMaxChannelValidator;
begin
  try
    LMinMaxFlowFeature      := FAppModules.Language.GetString('TYieldModelWizard.MinMaxFlowFeature');
    LMinMaxFlowFeatureDescr := FAppModules.Language.GetString('TYieldModelWizard.MinMaxFlowFeatureDescr');
    lStep := AddStep(6, 0, LMinMaxFlowFeature, LMinMaxFlowFeatureDescr, TMinMaxChannelValidator);
    lMinMaxValidator := TMinMaxChannelValidator(lStep.Validator);
    with lMinMaxValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID    := IMinMaxFlowConstraint(AChannel.MinMaxFlowConstraint).FeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadPumpingFeatureSteps (ADialogParent : TWinControl;
                                                     AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadPumpingFeatureSteps';
var
  lStep                : TWizardStep;
  LPumpingFeature      : string;
  LPumpingFeatureDescr : string;
  lPumpingValidator    : TPumpingFeatureValidator;
begin
  try
   LPumpingFeature      := FAppModules.Language.GetString('TYieldModelWizard.PumpingFeature');
   LPumpingFeatureDescr := FAppModules.Language.GetString('TYieldModelWizard.PumpingFeatureDescr');
    lStep := AddStep(7, 0, LPumpingFeature, LPumpingFeatureDescr, TPumpingFeatureValidator);
    lPumpingValidator := TPumpingFeatureValidator(lStep.Validator);
    with lPumpingValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID    := IPumpingFeature(AChannel.PumpingFeature).FeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadSpecifiedDemandFeatureSteps (ADialogParent : TWinControl;
                                                             AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadSpecifiedDemandFeatureSteps';
var
  lStep                        : TWizardStep;
  LSpecifiedDemandFeature      : string;
  LSpecifiedDemandFeatureDescr : string;
  lDemandValidator             : TSpecifiedDemandChannelValidator;
begin
  try
    LSpecifiedDemandFeature      := FAppModules.Language.GetString('TYieldModelWizard.SpecifiedDemandFeature');
    LSpecifiedDemandFeatureDescr := FAppModules.Language.GetString('TYieldModelWizard.SpecifiedDemandFeatureDescr');
    lStep := AddStep(8, 0, LSpecifiedDemandFeature, LSpecifiedDemandFeatureDescr, TSpecifiedDemandChannelValidator);
    lDemandValidator := TSpecifiedDemandChannelValidator(lStep.Validator);
    with lDemandValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID    := ISpecifiedDemandFeature(AChannel.SpecifiedDemandFeature).FeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadPhysicalFlowConstraintSteps (ADialogParent : TWinControl;
                                                             AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadPhysicalFlowConstraintSteps';
var
  lStep                        : TWizardStep;
  LPhysicalFlowConstraint      : string;
  LPhysicalFlowConstraintDescr : string;
  lValidator                   : TPhysicalFlowConstraintValidator;
begin
  try
    LPhysicalFlowConstraint      := FAppModules.Language.GetString('TYieldModelWizard.PhysicalFlowConstraint');
    LPhysicalFlowConstraintDescr := FAppModules.Language.GetString('TYieldModelWizard.PhysicalFlowConstraintDescr');
    lStep := AddStep(9, 0, LPhysicalFlowConstraint, LPhysicalFlowConstraintDescr, TPhysicalFlowConstraintValidator);
    lValidator := TPhysicalFlowConstraintValidator(lStep.Validator);
    with lValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID    := IPhysicalFlowConstraint(AChannel.PhysicalFlowConstraint).FeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadIFRFeatureSteps (ADialogParent : TWinControl;
                                                 AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadIFRFeatureSteps';
var
  lStep                     : TWizardStep;
  LIFRFeature               : string;
  LIFRRefereneceNodes       : string;
  LIFRRefereneceNodesDescr  : string;
  LIFRInflowAndRelease      : string;
  LIFRInflowAndReleaseDescr : string;
  lIFRValidator             : TIFRFeatureValidator;
begin
  try
    LIFRFeature := FAppModules.Language.GetString('TYieldModelWizard.IFRFeature');
    lStep := AddStep(10, 0, LIFRFeature, '', TIFRFeatureValidator);
    lIFRValidator := TIFRFeatureValidator(lStep.Validator);
    with lIFRValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID    := IIFRFeature(AChannel.IFRFeature).FeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
    LIFRRefereneceNodes       := FAppModules.Language.GetString('TYieldModelWizard.IFRRefereneceNodes');
    LIFRRefereneceNodesDescr  := FAppModules.Language.GetString('TYieldModelWizard.IFRRefereneceNodesDescr');
    LIFRInflowAndRelease      := FAppModules.Language.GetString('TYieldModelWizard.IFRInflowAndRelease');
    LIFRInflowAndReleaseDescr := FAppModules.Language.GetString('TYieldModelWizard.IFRRefereneceNodesDescr');
    AddStep(11, 10, LIFRRefereneceNodes, LIFRRefereneceNodesDescr, nil, 1);
    AddStep(12, 10, LIFRInflowAndRelease, LIFRInflowAndReleaseDescr, nil, 2);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadDiversionFeatureSteps (ADialogParent : TWinControl;
                                                       AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadDiversionFeatureSteps';
var
  lStep           : TWizardStep;
  LDiversion      : string;
  LDiversionDescr : string;
  lValidator      : TDiversionFeatureValidator;
begin
  try
    LDiversion      := FAppModules.Language.GetString('TYieldModelWizard.DiversionFeature');
    LDiversionDescr := FAppModules.Language.GetString('TYieldModelWizard.DiversionFeatureDescr');
    lStep := AddStep(13, 0, LDiversion, LDiversionDescr, TDiversionFeatureValidator);
    lValidator := TDiversionFeatureValidator(lStep.Validator);
    with lValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID := IDiversionFeature(AChannel.DiversionFeature).FeatureID;
      if (FeatureID <> 0) then
      begin
        PopulateDataViewer;
        LanguageHasChanged;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadSpecifiedInflowFeatureSteps (ADialogParent : TWinControl;
                                                             AChannel      : IGeneralFlowChannel);
const OPNAME = 'TYieldModelWizard.LoadSpecifiedInflowFeatureSteps';
var
  lStep                        : TWizardStep;
  LSpecifiedInflowFeature      : string;
  LSpecifiedInflowFeatureDescr : string;
  lValidator                   : TSpecifiedInflowDataValidator;
begin
  try
    LSpecifiedInflowFeature      := FAppModules.Language.GetString('TYieldModelWizard.SpecifiedInflowFeature');
    LSpecifiedInflowFeatureDescr := FAppModules.Language.GetString('TYieldModelWizard.SpecifiedInflowFeatureDescr');
    lStep := AddStep(14, 0, LSpecifiedInflowFeature, LSpecifiedInflowFeatureDescr, TSpecifiedInflowDataValidator);
    lValidator := TSpecifiedInflowDataValidator(lStep.Validator);
    with lValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID    := ISpecifiedInflowFeature(AChannel.SpecifiedInflowFeature).FeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadNodeWizard (ADialogParent : TWinControl;
                                            ANodeNumber   : integer);
const OPNAME = 'TYieldModelWizard.LoadNodeWizard';
var
  lStep                       : TWizardStep;
  LLoadNodeProperties         : string;
  LLoadNodePropertiesDescr    : string;
  LHydrologicalCatchment      : string;
  LHydrologicalCatchmentDescr : string;
  lValidator1                 : TNodePropertiesValidator;
  lValidator2                 : TReservoirCatchmentProportionsValidator;
begin
  try
    FSetFinishButtonState     := DoSetCloseButtonState;
    FFinishButtonCaption      := FAppModules.Language.GetString('ButtonCaption.Close');
    LLoadNodeProperties       := FAppModules.Language.GetString('TYieldModelWizard.NodeProperties');
    LLoadNodePropertiesDescr := FAppModules.Language.GetString('TYieldModelWizard.NodePropertiesDescr');
    lStep := AddStep(1, 0, LLoadNodeProperties, LLoadNodePropertiesDescr, TNodePropertiesValidator);
    lValidator1 := TNodePropertiesValidator(lStep.Validator);
    with lValidator1 do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      Identifier := ANodeNumber;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
    LHydrologicalCatchment      := FAppModules.Language.GetString('TYieldModelWizard.HydrologicalCatchment');
    LHydrologicalCatchmentDescr := FAppModules.Language.GetString('TYieldModelWizard.HydrologicalCatchmentDescr');
    lStep := AddStep(2, 0, LHydrologicalCatchment, LHydrologicalCatchmentDescr, TReservoirCatchmentProportionsValidator);
    lValidator2 := TReservoirCatchmentProportionsValidator(lStep.Validator);
    with lValidator2 do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      Identifier := ANodeNumber;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadReservoirWizard (ADialogParent    : TWinControl;
                                                 AReservoirNumber : integer);
const OPNAME = 'TYieldModelWizard.LoadReservoirWizard';
var
  lStep                        : TWizardStep;
  LReservoirProperties         : string;
  LReservoirPropertiesDescr    : string;
  LReservoirEvaporation        : string;
  LReservoirEvaporationDescr   : string;
  LPhysicalCharacteristics     : string;
//  LStoragecharacteristics      : string;
//  LStoragecharacteristicsDescr : string;
//  LSurfaceArea                 : string;
//  LSurfaceAreaDescr            : string;
  LHydrologicalCatchment       : string;
  LHydrologicalCatchmentDescr  : string;
  LZoneElevations              : string;
  LZoneElevationsDescr         : string;
  LReservoirPenalties          : string;
  LReservoirPenaltiesDescr     : string;
  lValidator1                  : TReservoirPropertiesValidator;
  lValidator2                  : TReservoirEvaporationValidator;
  lValidator3                  : TReservoirPhysicalCharacteristicsValidator;
  lValidator4                  : TReservoirCatchmentProportionsValidator;
  lValidator5                  : TReservoirPenaltyValidator;
  lValidator6                  : TReservoirZoneElevationsValidator;
begin
  try
    FSetFinishButtonState := DoSetCloseButtonState;
    FFinishButtonCaption  := FAppModules.Language.GetString('ButtonCaption.Close');

    LReservoirProperties      := FAppModules.Language.GetString('TYieldModelWizard.ReservoirProperties');
    LReservoirPropertiesDescr := FAppModules.Language.GetString('TYieldModelWizard.ReservoirPropertiesDescr');
    lStep := AddStep(1, 0, LReservoirProperties, LReservoirPropertiesDescr, TReservoirPropertiesValidator);
    lValidator1 := TReservoirPropertiesValidator(lStep.Validator);
    with lValidator1 do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      Identifier := AReservoirNumber;
      PopulateDataViewer;
      LanguageHasChanged;
    end;

    LReservoirEvaporation      := FAppModules.Language.GetString('TYieldModelWizard.ReservoirEvaporation');
    LReservoirEvaporationDescr := FAppModules.Language.GetString('TYieldModelWizard.ReservoirEvaporationDescr');
    lStep := AddStep(2, 0, LReservoirEvaporation, LReservoirEvaporationDescr, TReservoirEvaporationValidator);
    lValidator2 := TReservoirEvaporationValidator(lStep.Validator);
    with lValidator2 do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      Identifier := AReservoirNumber;
      PopulateDataViewer;
      LanguageHasChanged;
    end;

    LPhysicalCharacteristics := FAppModules.Language.GetString('TYieldModelWizard.PhysicalCharacteristics');
    lStep := AddStep(3, 0, LPhysicalCharacteristics, '', TReservoirPhysicalCharacteristicsValidator);
    lValidator3 := TReservoirPhysicalCharacteristicsValidator(lStep.Validator);
    with lValidator3 do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      Identifier := AReservoirNumber;
      PopulateDataViewer;
      LanguageHasChanged;
    end;

   {LStoragecharacteristics      := FAppModules.Language.GetString('TYieldModelWizard.StorageCharacteristics');
    LStoragecharacteristicsDescr := FAppModules.Language.GetString('TYieldModelWizard.StorageCharacteristicsDescr');
    LSurfaceArea                 := FAppModules.Language.GetString('TYieldModelWizard.SurfaceArea');
    LSurfaceAreaDescr            := FAppModules.Language.GetString('TYieldModelWizard.SurfaceAreaDescr');
    AddStep(4, 3, LStoragecharacteristics, LStoragecharacteristicsDescr, nil, 1);
    AddStep(5, 3, LSurfaceArea, LSurfaceAreaDescr, nil, 2);}


    LHydrologicalCatchment      := FAppModules.Language.GetString('TYieldModelWizard.HydrologicalCatchment');
    LHydrologicalCatchmentDescr := FAppModules.Language.GetString('TYieldModelWizard.HydrologicalCatchmentDescr');
    lStep := AddStep(6, 0, LHydrologicalCatchment, LHydrologicalCatchmentDescr, TReservoirCatchmentProportionsValidator);
    lValidator4 := TReservoirCatchmentProportionsValidator(lStep.Validator);
    with lValidator4 do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      Identifier := AReservoirNumber;
      PopulateDataViewer;
      LanguageHasChanged;
    end;

    LReservoirPenalties      := FAppModules.Language.GetString('TYieldModelWizard.LReservoirPenalties');
    LReservoirPenaltiesDescr := FAppModules.Language.GetString('TYieldModelWizard.LReservoirPenaltiesDescr');
    lStep := AddStep(7, 0, LReservoirPenalties, LReservoirPenaltiesDescr, TReservoirPenaltyValidator);
    lValidator5 := TReservoirPenaltyValidator(lStep.Validator);
    with lValidator5 do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      ReservoirNumber := AReservoirNumber;
      lValidator5.ViewMode := vmEditableSelect;
      PopulateDataViewer;
      LanguageHasChanged;
    end;

    LZoneElevations      := FAppModules.Language.GetString('TYieldModelWizard.ZoneElevations');
    LZoneElevationsDescr := FAppModules.Language.GetString('TYieldModelWizard.ZoneElevationsDescr');
    lStep := AddStep(8, 0, LZoneElevations, LZoneElevationsDescr, TReservoirZoneElevationsValidator);
    lValidator6 := TReservoirZoneElevationsValidator(lStep.Validator);
    with lValidator6 do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      Identifier := AReservoirNumber;
      PopulateDataViewer;
      LanguageHasChanged;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadIrrigationAreaWizard (ADialogParent : TWinControl;
                                                      AFeatureID    : integer);
const OPNAME = 'TYieldModelWizard.LoadIrrigationAreaWizard';
var
  lStep                 : TWizardStep;
  lIrrigationArea       : IIrrigationArea;
  lIrrigationValidator  : TIrrigationAreaValidator;
  lDiversionValidator   : TGeneralFlowChannelValidator;
  lConsumptiveValidator : TGeneralFlowChannelValidator;
  lReturnFlowValidator  : TGeneralFlowChannelValidator;
begin
  try
    lIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).
                         NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByID[AFeatureID];
    try
      if (lIrrigationArea <> nil) then
      begin
        FSetFinishButtonState := DoSetCloseButtonState;
        FFinishButtonCaption  := FAppModules.Language.GetString('ButtonCaption.Close');
        lStep := AddStep(1, 0, 'Irrigation Area',
                         '', TIrrigationAreaValidator);
        lIrrigationValidator := TIrrigationAreaValidator(lStep.Validator);
        with lIrrigationValidator do
        begin
          Panel.Parent := ADialogParent;
          Initialise;
          Identifier := lIrrigationArea.IrrigationNodeNumber;
          PopulateDataViewer;
          LanguageHasChanged;
        end;
        AddStep(2, 1, 'Diversion Channel Upstream Node',
                'Diversion channel upstream node must be defined and not zero', nil, 1);
        AddStep(3, 1, 'Return-flow Channel Downstream Node',
                'Return-flow channel downstream node must be defined', nil, 2);
        AddStep(4, 1, 'Monthly Diversion and Return Flows',
                'Monthly diversion and return flows must be defined', nil, 3);

        lStep := AddStep(5, 0, 'Diversion Channel Properties',
                         'Diversion channel must have a valid 2-arc penalty steructure', TGeneralFlowChannelValidator, 2);
        lDiversionValidator := TGeneralFlowChannelValidator(lStep.Validator);
        with lDiversionValidator do
        begin
          Panel.Parent := ADialogParent;
          Initialise;
          if (lIrrigationArea.DiversionChannel <> nil) then
            Identifier := lIrrigationArea.DiversionChannel.ChannelNumber;
          UpStreamEnabled   := FALSE;
          DownStreamEnabled := FALSE;
          PopulateDataViewer;
          LanguageHasChanged;
        end;

        lStep := AddStep(6, 0, 'Consumptive Channel Properties',
                         'Consumptive channel must have a valid 1-arc penalty structure', TGeneralFlowChannelValidator, 2);
        lConsumptiveValidator := TGeneralFlowChannelValidator(lStep.Validator);
        with lConsumptiveValidator do
        begin
          Panel.Parent := ADialogParent;
          Initialise;
          if (lIrrigationArea.ConsumptiveChannel <> nil) then
            Identifier := lIrrigationArea.ConsumptiveChannel.ChannelNumber;
          UpStreamEnabled   := FALSE;
          DownStreamEnabled := FALSE;
          PopulateDataViewer;
          LanguageHasChanged;
        end;

        lStep := AddStep(7, 0, 'Return-flow Channel Properties',
                         'Return-flow channel must have a valid 2-arc penalty structure', TGeneralFlowChannelValidator, 2);
        lReturnFlowValidator := TGeneralFlowChannelValidator(lStep.Validator);
        with lReturnFlowValidator do
        begin
          Panel.Parent := ADialogParent;
          Initialise;
          if (lIrrigationArea.ReturnFlowChannel <> nil) then
            Identifier := lIrrigationArea.ReturnFlowChannel.ChannelNumber;
          UpStreamEnabled   := FALSE;
          DownStreamEnabled := FALSE;
          PopulateDataViewer;
          LanguageHasChanged;
        end;
      end
    finally
      lIrrigationArea := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadPowerPlantWizard (ADialogParent : TWinControl;
                                                  AFeatureID    : integer);
const OPNAME = 'TYieldModelWizard.LoadPowerPlantWizard';
var
  lStep                  : TWizardStep;
  lPowerPlant            : IPowerPlant;
  lPowerPlantValidator   : TPowerPlantValidator;
  lPowerChannelValidator : TGeneralFlowChannelValidator;
  lSpillChannelValidator : TGeneralFlowChannelValidator;
  lFactorsValidator      : TPowerPlantFactorsValidator;
  lTailwaterValidator    : TPowerPlantTailwaterValidator;
  lDemandsValidator      : TPowerPlantDemandsValidator;
begin
  try
    FSetFinishButtonState := DoSetCloseButtonState;
    FFinishButtonCaption  := FAppModules.Language.GetString('ButtonCaption.Close');
    lStep := AddStep(1, 0, 'Power Plant',
                     '', TPowerPlantValidator);
    lPowerPlantValidator := TPowerPlantValidator(lStep.Validator);
    with lPowerPlantValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      FeatureID := AFeatureID;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
    AddStep(2, 1, 'Power/Spill Channel Upstream Node',
            'Power & spill channel upstream node must be defined and not zero', nil, 1);
    AddStep(3, 1, 'Power Channel Downstream Node',
            'Power channel downstream node must be defined', nil, 2);
    AddStep(4, 1, 'Spill Channel Downstream Node',
            'Spill channel downstream node must be defined', nil, 3);
    AddStep(5, 1, 'Capacity and Head Loss Values',
            'Maximum generator capacity, maximum turbine capacity and head loss must be defined', nil, 4);

    try
      lPowerPlant := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.PowerPlantList.PowerPlantByID[AFeatureID];
      if (lPowerPlant <> nil) then
      begin
        lStep := AddStep(6, 0, 'Power Channel Properties',
                         'Power channel must have a valid 3-arc penalty structure', TGeneralFlowChannelValidator, 2);
        lPowerChannelValidator := TGeneralFlowChannelValidator(lStep.Validator);
        with lPowerChannelValidator do
        begin
          Panel.Parent := ADialogParent;
          Initialise;
          if (lPowerPlant.PowerChannel <> nil) then
            Identifier := lPowerPlant.PowerChannel.ChannelNumber;
          UpStreamEnabled   := FALSE;
          DownStreamEnabled := FALSE;
          PopulateDataViewer;
          LanguageHasChanged;
        end;

        lStep := AddStep(7, 0, 'Spill Channel Properties',
                         'Spill channel must have a valid 1-arc penalty structure', TGeneralFlowChannelValidator, 2);
        lSpillChannelValidator := TGeneralFlowChannelValidator(lStep.Validator);
        with lSpillChannelValidator do
        begin
          Panel.Parent := ADialogParent;
          Initialise;
          if (lPowerPlant.SpillChannel <> nil) then
            Identifier := lPowerPlant.SpillChannel.ChannelNumber;
          UpStreamEnabled   := FALSE;
          DownStreamEnabled := FALSE;
          PopulateDataViewer;
          LanguageHasChanged;
        end;

        lStep := AddStep(8, 0, 'Efficiency and Net Head',
                         'Efficiency factors, combined efficiency, design head, maximum ' +
                         'and minimum net head as well as net head factors must be specified.',
                         TPowerPlantFactorsValidator);
        lFactorsValidator := TPowerPlantFactorsValidator(lStep.Validator);
        with lFactorsValidator do
        begin
          Panel.Parent := ADialogParent;
          Initialise;
          FeatureID := AFeatureID;
          PopulateDataViewer;
          LanguageHasChanged;
        end;

        lStep := AddStep(9, 0, 'Tailwater Discharge and Elevation',
                         'Discharge and tailwater elevation values must be specified.',
                         TPowerPlantTailwaterValidator);
        lTailwaterValidator := TPowerPlantTailwaterValidator(lStep.Validator);
        with lTailwaterValidator do
        begin
          Panel.Parent := ADialogParent;
          Initialise;
          FeatureID := AFeatureID;
          PopulateDataViewer;
          LanguageHasChanged;
        end;

        lStep := AddStep(10, 0, 'Power Generation and Release',
                         'Power generation and release values must be specified.',
                         TPowerPlantDemandsValidator);
        lDemandsValidator := TPowerPlantDemandsValidator(lStep.Validator);
        with lDemandsValidator do
        begin
          Panel.Parent := ADialogParent;
          Initialise;
          FeatureID := AFeatureID;
          PopulateDataViewer;
          LanguageHasChanged;
        end;

      end
    finally
      lPowerPlant := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadYieldRunHistoricWizard (ADialogParent : TWinControl);
const OPNAME = 'TYieldModelWizard.LoadYieldRunHistoricWizard';
var
  lStep      : TWizardStep;
  lValidator : TYieldRunHistoricValidator;
begin
  try
    FSetFinishButtonState := DoSetRunButtonState;
    FFinishButtonCaption  := FAppModules.Language.GetString('ButtonCaption.Run');
    FOnFinishButtonClick := DoRunYieldFinishClick;
    lStep := AddStep(1, 0, 'Run Yield Model Historic Mode', '', TYieldRunHistoricValidator);
    lValidator := TYieldRunHistoricValidator(lStep.Validator);
    with lValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
    AddStep(2, 1, 'Level of summary output',
            'Level of summary output is usually set to full summary.', nil, 1);
    AddStep(3, 1, 'Historic firm yield',
            'Firm yield analysis is usually selected for a historic run.', nil, 2);
    AddStep(4, 1, 'Number of years in simulation',
            'Number of years in simulation is usually equal to the number ' +
            'of years in the hydrological sequence.', nil, 3);
    AddStep(5, 1, 'Start month number',
            'Start month number is usually set to 1, i.e. October.', nil, 4);
    AddStep(6, 1, 'Load cases',
            'At least one target draft must be specified and included in analysis.', nil, 5);
    AddStep(7, 1, 'Specified demand indicators ',
            'Specified demand indicators must all be set to the same value ' +
            'which is usually Historic (H).', nil, 6);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadYieldRunStochasticWizard (ADialogParent : TWinControl);
const OPNAME = 'TYieldModelWizard.LoadYieldRunStochasticWizard';
var
  lStep      : TWizardStep;
  lValidator : TYieldRunStochasticValidator;
begin
  try
    FSetFinishButtonState := DoSetRunButtonState;
    FFinishButtonCaption  := FAppModules.Language.GetString('ButtonCaption.Run');
    FOnFinishButtonClick := DoRunYieldFinishClick;
    lStep := AddStep(1, 0, 'Run Yield Model Stochastic Mode', '', TYieldRunStochasticValidator);
    lValidator := TYieldRunStochasticValidator(lStep.Validator);
    with lValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
    AddStep(2, 1, 'Level of summary output',
            'Level of summary output is usually set to brief summary.', nil, 1);
    AddStep(3, 1, 'Multiple period lengths',
            'Multiple period lengths is usually not selected for sequence lengths ' +
            'greater than 10.', nil, 2);
    AddStep(4, 1, 'Reduce number of sequences',
            'Reduce number of sequences is usually selected for a stochastic run.', nil, 3);
    AddStep(5, 1, 'Method of stochastic generation',
            'Method of stochastic generation is usually set to historic.', nil, 4);
    AddStep(6, 1, 'Number of sequences in analysis',
            'Number of sequences in analysis for a stochastic run.', nil, 5);
    AddStep(7, 1, 'Order of sequences to be analysed',
            'Order of sequences to be analysed. Up to 10 sequences that must be ' +
            'analysed out of sequence may be specified. Zeroes imply that sequences ' +
            'are analysed in consecutive order.', nil, 6);
    AddStep(8, 1, 'Number of years in simulation',
            'Number of years in simulation is usually equal to the number ' +
            'of years in the hydrological sequence.', nil, 7);
    AddStep(9, 1, 'Load cases',
            'At least one target draft must be specified and included in analysis.', nil, 8);
    AddStep(10, 1, 'Specified demand indicators ',
            'Specified demand indicators must all be set to the same value ' +
            'which is usually Stochastic (S).', nil, 9);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.LoadYieldRunYRCWizard (ADialogParent : TWinControl);
const OPNAME = 'TYieldModelWizard.LoadYieldRunYRCWizard';
var
  lStep      : TWizardStep;
  lValidator : TYieldRunYRCValidator;
begin
  try
    FSetFinishButtonState := DoSetRunButtonState;
    FFinishButtonCaption  := FAppModules.Language.GetString('ButtonCaption.Run');
    FOnFinishButtonClick := DoRunYieldFinishClick;
    lStep := AddStep(1, 0, 'Run Yield Model YRC Mode', '', TYieldRunYRCValidator);
    lValidator := TYieldRunYRCValidator(lStep.Validator);
    with lValidator do
    begin
      Panel.Parent := ADialogParent;
      Initialise;
      PopulateDataViewer;
      LanguageHasChanged;
    end;
    AddStep(2, 1, 'Level of summary output',
            'Level of summary output must be set to brief summary.', nil, 1);
    AddStep(3, 1, 'Historic firm yield',
            'Firm yield analysis must not be selected for YRC.', nil, 2);
    AddStep(4, 1, 'Multiple period lengths',
            'Multiple period lengths is usually not selected for sequence lengths ' +
            'greater than 10.', nil, 3);
    AddStep(6, 1, 'Number of years in simulation',
            'If multiple period lengths is selected then number of years in simulation ' +
            'should be less than or equal to 10. If number of years is greater than 10 ' +
            'multiple period lengths should not be selected.', nil, 4);
    AddStep(6, 1, 'Load cases',
            'At least one target draft must be specified and included in analysis. ' +
            'Number of load cases should be greater than 3 to get a reasonable ' +
            'definition of the firm yield line.', nil, 5);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.DoSetCloseButtonState (Sender : TObject);
const OPNAME = 'TYieldModelWizard.DoSetCloseButtonState';
begin
  try
    if (Sender.ClassNameIs('TButton')) then
      TButton(Sender).Enabled := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.DoSetRunButtonState (Sender : TObject);
const OPNAME = 'TYieldModelWizard.DoSetRunButtonState';
var
  lIndex     : integer;
  lStep      : TWizardStep;
  lRun       : Boolean;
begin
  try
    if (Sender.ClassNameIs('TButton')) then
    begin
      lRun := TRUE;
      lIndex := 0;
      while (lRun AND (lIndex < FStepList.Count)) do
      begin
        lStep := FStepList.Items[lIndex];
        if (lStep.IsStepOK) then
          lIndex := lIndex + 1
        else
          lRun := FALSE;
      end;
      TButton(Sender).Enabled := lRun;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldModelWizard.DoRunYieldFinishClick (Sender : TObject);
const OPNAME = 'TYieldModelWizard.DoRunYieldFinishClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeExportFilesAndRunModel,nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
