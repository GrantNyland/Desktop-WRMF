//
//  UNIT      : Contains TYieldModelDataGUIManager Class
//  AUTHOR    : Valentino Naicker(ARIVIA)
//  DATE      : 07/04/2003
//  COPYRIGHT : Copyright © 2003 DWAF
//
//  GUI's are managed on 2 levels  Owned or referenced
//  When referenced the Owner is the Application MainForm as per the Design
//

unit UPlanningModelDataGUIManager;

interface

uses

  // Delphi
  classes,
  contnrs,
  VCL.controls,
  VCL.dialogs,

  // DWAF
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UYieldModelDataGUIForm,
  UYieldModelDataGUIManager,
  VoaimsCom_TLB;

type
  TPlanningModelDataGUIManager = class(TYieldModelDataGUIManager)
  protected
    procedure DestroyMemberObjects; override;
    function InitialiseOutputDataSelector(ANetworkElementType : TNetworkElementType): boolean;
    function ViewInputData(ACommaTextContextData : String; APageControl   : TAbstractDataPageControl): boolean; override;
    function ViewOutputData(ACommaTextContextData : String; APageControl   : TAbstractDataPageControl): boolean;  override;
    procedure DoOnDataChange(Sender : TObject);
    procedure DoOnIncludeMinMaxDataChange(Sender : TObject);
  public
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
  end;

implementation

uses

  // Delphi
  VCL.Forms,
  Sysutils,
  Windows,

  // DWAF
  UConstants,
  UErrorHandlingOperations,
  UPlanningModelDataObject,

  UFMAllocationDefinitionValidator,
  USwitchDefinitionValidator,
  UFMUserPriorityClassificationValidator,
  UFMYieldCharacteristicsValidator,
  UFMSupportStrategyValidator,
  UFMSubSystemsValidator,
  UFMDemandDefinitionValidator,
  UFMSupportChannelsValidator,
  UPlanningMineUnderGroundSectionValidator,
  UPlanningRunConfigurationValidator,
  UGrowthProjectionsValidator,
  UGrowthFactorsValidator,
  UDisbenefitFunctionDefinitionDataValidator,
  UReturnFlowChannelValidator,
  UTariffCalculationDataValidator,
  UChannelGrowthValidator,
  UHydroGaugeGrowthValidator,
  UGrowthFactorsGraphValidator,
  UGeneralFlowChannelValidator,
  UMinMaxChannelValidator,
  UWQConstraintValidator,
  UChannelPenaltyValidator,
  UMultiResChannelValidator,
  UMiningSlurryDumpValidator,
  UMiningUnderGroundSectionValidator,
  UPlanningMineValidator,
  UPlanningMineOpenCastValidator,
  UPlanningSlurryDumpValidator,
  //UOutputWRPMGraphValidator,
  UOutputCollateFilesValidator,
  UOutputWRPMGridValidator,
  UOutputWRPMBoxPlotGridValidator,
  UOutputWRPMBoxPlotGraphValidator;

{ TPlanningModelDataGUIManager }
function TPlanningModelDataGUIManager.ViewInputData(ACommaTextContextData: String; APageControl   : TAbstractDataPageControl): boolean;
const OPNAME = 'TPlanningModelDataGUIManager.ViewInputData';
var
  LElementType        : string;
  LViewName           : string;
  LIdentifier         : integer;
  LContextDataList    : TStringList;
  LDialogValidator    : TAbstractDataDialogValidator;
  lRunConfigData      : IRunConfigurationData;
  LDisbenefitFunction : IDisbenefitFunctionDefinition;
  LReturnFlowChannel  : IReturnFlowChannel;
  LChannelTariff      : IChannelTariff;
  LChannel            : IGeneralFlowChannel;
  LChannelType        : integer;
  lMasterControlFeature : IMasterControlFeature;
  LDemandCentreGrowthFactors  : IDemandCentreGrowthFactors;
  LMinMaxGrowthFactors : IMinMaxChannelGrowthFactors;
  LWQConstraintData    : IWQConstraintData;
  LWQConstriantsChannel : IWQConstriantsChannel;
  LMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel;
  LMultiResChannelCurtail  : IMultiResMultiChannelCurtail;
  LMine                       : IPlanningMine;
begin
  Result := inherited ViewInputData(ACommaTextContextData,APageControl);
  try
    if (Trim(ACommaTextContextData) = '') then Exit;

    LContextDataList := TStringList.Create;
    try
      LContextDataList.CommaText := ACommaTextContextData;
      LViewName := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
      if(LViewName = '') then Exit;

      LIdentifier   := StrToInt(LContextDataList.Values['MODELELEMENTID']);
      LElementType  := UpperCase(Trim(LContextDataList.Values['MODELELEMENTTYPE']));
    finally
      FreeAndNil(LContextDataList);
    end;
    lRunConfigData := TPlanningModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if ((LViewName = mdvnChannel) OR (LViewName = mdvnMasterControlConfiguration)) then
    begin
      LChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[LIdentifier];
      if Assigned(LChannel) then
      begin
        LChannelType := LChannel.ChannelType;

        if (LChannelType in [1,13]) then Exit;

        lMasterControlFeature := LChannel.MasterControlFeature;
        LDisbenefitFunction   := lChannel.DisbenefitFunction;
        LReturnFlowChannel    := lChannel.ReturnFlowChannel;
        LChannelTariff        := lChannel.TariffCalculation;

        LDemandCentreGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).
                                      CastGrowthFactors.DemandGrowthFactorsByChannel[LChannel.ChannelNumber];
        LMinMaxGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).
                                CastGrowthFactors.MinMaxChannelGrowthFactorsByMinMaxChannel[LChannel.ChannelNumber];
        //Master Control Feature
        LWQConstraintData := TPlanningModelDataObject(FAppModules.Model.ModelData).CastWQConstriantData;

        LMultiResChannelCurtail := TPlanningModelDataObject(FAppModules.Model.ModelData).
                                CastMultiRestrictionData.RestrictionByChannelNo[LChannel.ChannelNumber];



        if (lChannel.ChannelType = ctMinMaxChannel) then
        begin
          LDialogValidator := FInputPageControl.GetValidatorByClassName('TGeneralFlowChannelValidator');
          if (LDialogValidator <> nil) then
            LDialogValidator.OnDataChange := DoOnDataChange;

          LDialogValidator := FInputPageControl.GetValidatorByClassName('TMinMaxChannelValidator');
          if (LDialogValidator <> nil) then
            LDialogValidator.OnDataChange := DoOnIncludeMinMaxDataChange;

          if (LWQConstraintData <> nil) and (lRunConfigData.WaterQualityOption) then
          begin
            LMinMaxUpperBoundChannel := LWQConstraintData.MinMaxUpperBoundChannelNo[LChannel.ChannelNumber];
            LWQConstriantsChannel := LWQConstraintData.WQConstraintsChannelByChannelNo[LChannel.ChannelNumber];
            if (LMinMaxUpperBoundChannel <> nil) or (LWQConstriantsChannel <> nil) then
            begin
              LDialogValidator := TWQConstraintValidator.Create(nil,FAppModules);
              APageControl.AddValidator(LDialogValidator);
              TWQConstraintValidator(LDialogValidator).Identifier := lChannel.ChannelNumber;
              LDialogValidator.Initialise;
              LDialogValidator.PopulateDataViewer;
              LDialogValidator.LanguageHasChanged;
            end;
          end;
        end;




        if(lMasterControlFeature <> nil) then
        begin
          if (LDisbenefitFunction <> nil) then
          begin
            LDialogValidator := TDisbenefitFunctionDefinitionDataValidator.Create(nil,FAppModules);
            APageControl.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TDisbenefitFunctionDefinitionDataValidator(LDialogValidator).Identifier := lMasterControlFeature.Channel.ChannelNumber;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
          if (LReturnFlowChannel <> nil) then
          begin
            LDialogValidator := TReturnFlowChannelValidator.Create(nil,FAppModules);
            APageControl.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TReturnFlowChannelValidator(LDialogValidator).Identifier := lMasterControlFeature.Channel.ChannelNumber; //lMasterControlFeature.FeatureID;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
          if (LChannelTariff <> nil) then
          begin
            LDialogValidator := TTariffCalculationDataValidator.Create(nil,FAppModules);
            APageControl.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TTariffCalculationDataValidator(LDialogValidator).Identifier := LChannel.ChannelNumber;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;

        end;

        if(lChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel]) then
        begin

          if (LDemandCentreGrowthFactors <> nil) or (LMinMaxGrowthFactors <> nil) then
          begin
            LDialogValidator := TChannelGrowthValidator.Create(nil,FAppModules);
            APageControl.AddValidator(LDialogValidator);
            LDialogValidator.Initialise;
            TChannelGrowthValidator(LDialogValidator).Identifier := LChannel.ChannelNumber;
            LDialogValidator.PopulateDataViewer;
            LDialogValidator.LanguageHasChanged;
          end;
        end;

        if(LMultiResChannelCurtail <> nil) then
        begin
          LDialogValidator := TMultiResChannelValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Initialise;
          TMultiResChannelValidator(LDialogValidator).FeatureID := LMultiResChannelCurtail.Identifier;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        end;

      end;
    end;

    if (LViewName = mdvnRunConfiguration) then
    begin
      //Run Configuration
      if (lRunConfigData.HasBeenPopulated) then
      begin
        LDialogValidator := TPlanningRunConfigurationValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end
    end
    else
    if (LViewName = mdvnCatchmentProportions) then
    begin


     // Channel Penalty
      LDialogValidator := TChannelPenaltyValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      //  TChannelPenaltyValidator(LDialogValidator).ChannelNumber := LChannel.ChannelNumber;
      TChannelPenaltyValidator(lDialogValidator).ViewMode := vmEditableSelect;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;


      LDialogValidator := THydroGaugeGrowthValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
       //THydroGaugeGrowthValidator(LDialogValidator).Identifier := LChannel.ChannelNumber;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else if (LViewName = mdvnGrowthFactors) then
    begin
      //Growth Projections
      LDialogValidator := TGrowthProjectionsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      //Growth Factors
      LDialogValidator := TGrowthFactorsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end
    else if (LViewName = mdvnAllocationDefinition) then
    begin
      // Allocation Definition
      LDialogValidator := TFMAllocationDefinitionValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TFMAllocationDefinitionValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // User Priority Classification
      LDialogValidator := TFMUserPriorityClassificationValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TFMUserPriorityClassificationValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Short Term Yield Characteristics
      LDialogValidator := TFMYieldCharacteristicsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TFMYieldCharacteristicsValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Sub-systems
      LDialogValidator := TFMSubSystemsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TFMSubSystemsValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Support Strategy
      LDialogValidator := TFMSupportStrategyValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TFMSupportStrategyValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Support Channels
      LDialogValidator := TFMSupportChannelsValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TFMSupportChannelsValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      // Demand Support Definition
      LDialogValidator := TFMDemandDefinitionValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TFMDemandDefinitionValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnSwitchDefinition) then
    begin
      // Switch Definition
      LDialogValidator := TSwitchDefinitionValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TSwitchDefinitionValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
    end;

    if (LViewName = mdvnMine) then
    begin

      if APageControl.DeleteAllTabSheets then
      begin
        LMine :=  IPlanningMine(TPlanningModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastMineList.MineByNodeNumber[LIdentifier]);
        LDialogValidator := TPlanningMineValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TPlanningMineValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TGeneralFlowChannelValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        if (LMine <> nil) AND (LMine.RiverChannel <> nil) then
          TGeneralFlowChannelValidator(LDialogValidator).Identifier         := LMine.RiverChannel.ChannelNumber;
        TGeneralFlowChannelValidator(LDialogValidator).Heading              := 'NetworkFeatures.MineToRiverChannelProperties';
        TGeneralFlowChannelValidator(LDialogValidator).UpStreamEnabled      := False;
        TGeneralFlowChannelValidator(LDialogValidator).DownStreamEnabled    := True;
        TGeneralFlowChannelValidator(LDialogValidator).SelectChannelEnabled := False;
        TGeneralFlowChannelValidator(LDialogValidator).Mode                 := gfcvmMineToRiver;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


        LDialogValidator := TPlanningMineOpenCastValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        TPlanningMineOpenCastValidator(LDialogValidator).Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

      LDialogValidator := TPlanningMineUnderGroundSectionValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TMiningUnderGroundSectionValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;

      LDialogValidator := TPlanningSlurryDumpValidator.Create(nil,FAppModules);
      APageControl.AddValidator(LDialogValidator);
      LDialogValidator.Initialise;
      TPlanningSlurryDumpValidator(LDialogValidator).Identifier := LIdentifier;
      LDialogValidator.PopulateDataViewer;
      LDialogValidator.LanguageHasChanged;
      end;

    end;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataGUIManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TPlanningModelDataGUIManager.StudyDataHasChanged';
begin
  Result := False;
  try
    TPlanningModelDataObject(FAppModules.Model.ModelData).CastWRPMPostProcessorData.Populating := True;
    try
      Result := inherited StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);


    finally
      TPlanningModelDataObject(FAppModules.Model.ModelData).CastWRPMPostProcessorData.Populating := False;
    end;



  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TPlanningModelDataGUIManager.ViewOutputData(ACommaTextContextData: String; APageControl   : TAbstractDataPageControl): boolean;
const OPNAME = 'TPlanningModelDataGUIManager.ViewOutputData';
var
  LViewName           : string;
  LElementName        : string;
  LContextDataList    : TStringList;
  LDialogValidator    : TAbstractDataDialogValidator;
  LIdentifier         : integer;
  LNetworkElementType : TNetworkElementType;
  LOldCursor          : TCursor;
  LNodeIndex          : integer;
  //LChannel            : IGeneralFlowChannel;
begin
   Result := False;
  //Result := inherited ViewOutputData(ACommaTextContextData,APageControl);
  try
    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LockWindowUpdate(APageControl.Handle);
    TPlanningModelDataObject(FAppModules.Model.ModelData).CastWRPMPostProcessorData.Populating := True;
    try
      if (Trim(ACommaTextContextData) = '') then Exit;

      LContextDataList := TStringList.Create;
      try
        LContextDataList.CommaText := ACommaTextContextData;
        LViewName := UpperCase(Trim(LContextDataList.Values['VIEWNAME']));
        if(LViewName = '') then Exit;

        LIdentifier   := StrToIntDef(LContextDataList.Values['MODELELEMENTID'],NullInteger);
        if(LIdentifier = NullInteger) then Exit;
        LNodeIndex    := StrToIntDef(LContextDataList.Values['NODEINDEX'],-1);

        LElementName := LContextDataList.Values['MODELELEMENTNAME'];
        if(LElementName <> '') then
          LElementName := AnsiDequotedStr(LElementName,'''');
      finally
        FreeAndNil(LContextDataList);
      end;

      LNetworkElementType := votNone;
      if(LViewName = mdvnReviewCollateOutputFiles)     then LNetworkElementType := votNone;
      if(LViewName = mdvnMasterControlConfiguration)   then LNetworkElementType := votMasterControl;
      if(LViewName = mdvnReviewSubSystemStorage)       then LNetworkElementType := votReviewSubSystemStorage;
      if(LViewName = mdvnReviewTotalSystemStorage)     then LNetworkElementType := votReviewTotalSystemStorage;
      if(LViewName = mdvnReviewSubSystemCurtailment)   then LNetworkElementType := votReviewSubSystemCurtailment;
      if(LViewName = mdvnReviewTotalSystemCurtailment) then LNetworkElementType := votTotalSystemCurtailment;
      if(LViewName = mdvnReviewDemandSupply)           then LNetworkElementType := votReviewDemandSupply;
      if(LViewName = mdvnReviewInterBasinSupport)      then LNetworkElementType := votReviewInterBasinSupport;
      if(LViewName = mdvnReservoir)                    then LNetworkElementType := votReviewDamStorage;
      if(LViewName = mdvnChannel)                      then LNetworkElementType := votChannel;

      if(LViewName = mdvnReviewDamStorage)             then LNetworkElementType := votReviewDamStorage;
      if(LViewName = mdvnReviewMonthlyReservoirResult) then LNetworkElementType := votReviewMonthlyReservoirResult;
      if(LViewName = mdvnReviewMonthlyChannelResult)   then LNetworkElementType := votReviewMonthlyChannelResult;
      if(LViewName = mdvnReviewDemands)                then LNetworkElementType := votReviewDemands;
      if(LViewName = mdvnReviewSubSystems)             then LNetworkElementType := votReviewSubSystems;

      if(LNetworkElementType <> votNone) then
        InitialiseOutputDataSelector(LNetworkElementType);

      if (LViewName = mdvnReviewCollateOutputFiles) then
      begin
        LDialogValidator := TOutpuCollateFilesValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        Result := True;
        Exit;
      end
      else if (LViewName = mdvnMasterControlConfiguration)   or (LViewName = mdvnReservoir) or
              (LViewName = mdvnReviewSubSystemStorage)       or (LViewName = mdvnReviewTotalSystemStorage) or
              (LViewName = mdvnReviewSubSystemCurtailment)   or (LViewName = mdvnReviewTotalSystemCurtailment) or
              (LViewName = mdvnReviewDemandSupply)           or (LViewName = mdvnReviewInterBasinSupport)      then
      begin
        LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Identifier         := LIdentifier;
        LDialogValidator.ElementName        := LElementName;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Identifier         := LIdentifier;
        LDialogValidator.ElementName        := LElementName;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Identifier         := LIdentifier;
        LDialogValidator.ElementName        := LElementName;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.TreeNodeIndex      := LNodeIndex;
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        Result := True;
        Exit;
      end
      else if (LViewName = mdvnChannel) then
      begin
        //LChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
        //              NetworkElementData.ChannelList.ChannelByChannelNumber[LIdentifier];
        //if(LChannel <> nil) and (LChannel.RequiresFirmYieldAnalysis = 'Y') then
        //begin
          LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Identifier         := LIdentifier;
          LDialogValidator.ElementName        := LElementName;
          LDialogValidator.NetworkElementType := LNetworkElementType;
          LDialogValidator.Initialise;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Identifier         := LIdentifier;
          LDialogValidator.ElementName        := LElementName;
          LDialogValidator.NetworkElementType := LNetworkElementType;
          LDialogValidator.Initialise;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;

          LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
          APageControl.AddValidator(LDialogValidator);
          LDialogValidator.Identifier         := LIdentifier;
          LDialogValidator.ElementName        := LElementName;
          LDialogValidator.NetworkElementType := LNetworkElementType;
          LDialogValidator.Initialise;
          LDialogValidator.PopulateDataViewer;
          LDialogValidator.LanguageHasChanged;
        //end;
        Result := True;
        Exit;
      end;
      {else if (LViewName = mdvnReviewCurtailments) then
      begin
        LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        Result := True;
        Exit;
      end
      else
      if (LViewName = mdvnReviewDemandSupply) then
      begin
        LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end
      else
      if LViewName = mdvnReviewTotalSystemStorage then
      begin

        LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


        LDialogValidator := TOutputWRPMGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end
      else
      if LViewName = mdvnReviewInterBasinSupport then
      begin

        LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

      end
      else
      if (LViewName = mdvnReviewDamStorage) then
      begin

        LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

      end
      else
      if  (LViewName = mdvnReviewDemands) then
      begin

        LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;


      end
      else
      if (LViewName = mdvnReviewSubSystems)then
      begin

        LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

      end
      else
      if (LViewName = mdvnReviewMonthlyChannelResult)then
      begin
        LDialogValidator := TOutputWRPMGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGridValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;

        LDialogValidator := TOutputWRPMBoxPlotGraphValidator.Create(nil,FAppModules);
        APageControl.AddValidator(LDialogValidator);
        LDialogValidator.Initialise;
        LDialogValidator.Identifier := LIdentifier;
        LDialogValidator.NetworkElementType := LNetworkElementType;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
      end;}
      
      Result := True;
    finally
      LockWindowUpdate(0);
      Screen.Cursor := LOldCursor;
      TPlanningModelDataObject(FAppModules.Model.ModelData).CastWRPMPostProcessorData.Populating := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPlanningModelDataGUIManager.InitialiseOutputDataSelector(ANetworkElementType: TNetworkElementType): boolean;
const OPNAME = 'TPlanningModelDataGUIManager.InitialiseOutputDataSelector';
begin
  Result := False;
  try
    TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Initialise;
    if(ANetworkElementType = votReviewTotalSystemStorage) then
      TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.Set_TimeStep(otsAnnual);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPlanningModelDataGUIManager.DestroyMemberObjects;
const OPNAME = 'TPlanningModelDataGUIManager.DestroyMemberObjects';
begin
  try
      LastSelectedStringGrid.Free;
      inherited;
  except on E: Exception do HandleError(E,OPNAME); end;


end;

procedure TPlanningModelDataGUIManager.DoOnDataChange(Sender : TObject);
const OPNAME = 'TPlanningModelDataGUIManager.DoOnDataChange';
var
  LDialogValidator : TAbstractDataDialogValidator;
begin
  try
    if (Sender.ClassName = 'TGeneralFlowChannelValidator') then
    begin
      LDialogValidator := FInputPageControl.GetValidatorByClassName('TChannelGrowthValidator');
      if (LDialogValidator = nil) and
        ((Sender as TGeneralFlowChannelValidator).GeneralFlowChannelDialog.GrowthChkBox.Checked)  then
      begin
        LDialogValidator := TChannelGrowthValidator.Create(nil,FAppModules);
        FInputPageControl.AddValidator(LDialogValidator);
        LDialogValidator.Identifier := (Sender as TGeneralFlowChannelValidator).Identifier;
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        FInputPageControl.LanguageHasChanged;
      end
      else
      if (LDialogValidator <> nil) and not
      ((Sender as TGeneralFlowChannelValidator).Growth)  then
      FInputPageControl.DeleteValidatorByClassName('TChannelGrowthValidator');
    end;





  except on E: Exception do HandleError ( E, OPNAME ) end;
end;



procedure TPlanningModelDataGUIManager.DoOnIncludeMinMaxDataChange(Sender : TObject);
const OPNAME = 'TPlanningModelDataGUIManager.DoOnIncludeMinMaxDataChange';
var
  LDialogValidator : TAbstractDataDialogValidator;
begin
  try
    if (Sender.ClassName = 'TMinMaxChannelValidator') then
    begin
      LDialogValidator := FInputPageControl.GetValidatorByClassName('TWQConstraintValidator');
      if ((LDialogValidator = nil) ) and
        (((Sender as TMinMaxChannelValidator).MinMaxChannelDialog.IncludeInWQConChkBox.Checked) or
        ((Sender as TMinMaxChannelValidator).MinMaxChannelDialog.IncludeInBoundChkBox.Checked))  then
      begin
        LDialogValidator := TWQConstraintValidator.Create(nil,FAppModules);
        FInputPageControl.AddValidator(LDialogValidator);
        LDialogValidator.Identifier := (Sender as TMinMaxChannelValidator).Identifier;
        LDialogValidator.Initialise;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        FInputPageControl.LanguageHasChanged;
      end;
     { else
      if (LDialogValidator <> nil) and not
      (((Sender as TMinMaxChannelValidator).MinMaxChannelDialog.IncludeInWQConChkBox.Checked) or
        ((Sender as TMinMaxChannelValidator).MinMaxChannelDialog.IncludeInBoundChkBox.Checked))  then
      FInputPageControl.DeleteValidatorByClassName('TWQConstraintValidator');   }
    end;





  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

