{******************************************************************************}
{*  UNIT      : Contains the class TGeneralFlowChannelValidator.              *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/03                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UGeneralFlowChannelValidator;

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
  UChannelPenaltyValidator,
  USelectChannelValidator,
  UGeneralFlowChannelDialog,
  UGrowthFactorsExcelData,
  UGrowthFactorData,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TGeneralFlowChannelValidatorMode = (gfcvmChannel,
                                      gfcvmIrrBlockUpstream,
                                      gfcvmIrrBlockDownstream,
                                      gfcvmWetlandUpstream,
                                      gfcvmWetlandDownstream,
                                      gfcvmDemandCentreConsumptiveUpstream,
                                      gfcvmDemandCentreReclaimationUpstream,
                                      gfcvmMineToRiver,
                                      gfcvmMineToPCD,
                                      gfcvmMineToUndeground,
                                      gfcvmGroundWaterAquiferInflowChannel,
                                      gfcvmInflowFromUpstreamAquiferChannel,
                                      gfcvmOutflowToDownstreamAquiferChannel,
                                      gfcvmGroundWaterBaseflowChannel,
                                      gfcvmAbtstractionFromAquiferChannel,
                                      gfcvmAbtstractionGroundWaterBaseflowChannel,
                                      gfcvmExcessInterflowChannel,
                                      gfcvmSurfaceRunoffChannel,
                                      gfcvmGroundWaterBaseFlowRemainderChannel,
                                      gfcvmGroundWaterAbstractionChannel,
                                      gfcvmOutflowToNetworkChannel);
  TGeneralFlowChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading                 : string;
    FUpStreamEnabled         : Boolean;
    FDownStreamEnabled       : Boolean;
    FSelectChannelEnabled    : Boolean;
    FGrowth                  : boolean;
    FChannelPenaltyValidator : TChannelPenaltyValidator;
    FSelectChannelValidator  : TSelectChannelValidator;
    FMode:TGeneralFlowChannelValidatorMode;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnEditControlDoubleClick(ASender: TObject);

    procedure OnSummaryOutputClick(Sender : TObject);
    procedure DoGrowthChkBoxOnClick(Sender : TObject);
    procedure DoFlowOutputChkBoxOnClick(Sender : TObject);
    procedure UpdateChannelName;
    procedure UpdateChannelUpstreamNode;
    procedure UpdateChannelDownstreamNode;
    procedure UpdateChannelArea;
    procedure UpdateChannelPenaltyStructure; virtual;
    procedure UpdateChannelIncludeSummary; virtual;
    procedure UpdateChannelFlowOutput;
    procedure OnSelectPenaltyClick(Sender: TObject);
    procedure OnSelectChannelClick(Sender: TObject);
    procedure OnFirmYieldAnalysisClick(Sender: TObject);
    procedure UpdateChannelFirmYieldAnalysis;
    procedure PopulateFirmYieldAnalysis;
    procedure RePopulateDataViewer;
    procedure RePopulateNodes;
    procedure RepopulateChannelAreas;
    procedure ShowChannelPenaltyDialog (ASender : TObject);
    procedure ShowSelectChannelDialog (ASender : TObject);
    procedure ValidateChannelNumber (AChannel : IGeneralFlowChannel);
    procedure ValidateChannelName (AChannel : IGeneralFlowChannel);
    procedure ValidateChannelPenalty (AChannel : IGeneralFlowChannel);
    procedure ValidateUpAndDownstreamNodes (AChannelList : IChannelList;
                                            AChannel     : IGeneralFlowChannel);
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
    function GeneralFlowChannelDialog : TGeneralFlowChannelDialog;
    property Heading                  : string  read FHeading               write FHeading;
    property UpStreamEnabled          : Boolean read FUpStreamEnabled       write FUpStreamEnabled;
    property DownStreamEnabled        : Boolean read FDownStreamEnabled     write FDownStreamEnabled;
    property SelectChannelEnabled     : Boolean read FSelectChannelEnabled  write FSelectChannelEnabled;
    property Growth                   : boolean read FGrowth                write FGrowth;
    property Mode                     : TGeneralFlowChannelValidatorMode read FMode write FMode;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  VCL.Dialogs,
  UWRMFThemes,
  UIrrigationBlock,
  UWetland,
  UYieldModelDataGUIForm,
  UChannelPenaltyDialog,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UPlanningModelDataObject,
  USelectChannelDialog, UNetworkElementData, UNetworkFeaturesData,
  UMiningData, UReservoirData, UGroundWater;

{******************************************************************************}
{* TGeneralFlowChannelValidator                                               *}
{******************************************************************************}

procedure TGeneralFlowChannelValidator.CreateMemberObjects;
const OPNAME = 'TGeneralFlowChannelValidator.CreateMemberObjects';
var
  lpPanel     : TGeneralFlowChannelDialog;
begin
  try
    inherited CreateMemberObjects;
    FMode                    := gfcvmChannel;
    FHeading                 := 'NetworkFeatures.ChannelProperties';
    FUpStreamEnabled         := TRUE;
    FDownStreamEnabled       := TRUE;
    FSelectChannelEnabled    := FALSE;
    FGrowth                  := False;

    FChannelPenaltyValidator := nil;
    FSelectChannelValidator  := nil;
    CreateDialog;
    lpPanel := GeneralFlowChannelDialog;
    with lpPanel do
    begin
      ChannelNumberEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('ChannelNumber');
      ChannelNumberEdit.ReadOnly         := TRUE;
      ChannelNumberEdit.OnEnter          := OnEditControlEnter;
      ChannelNumberEdit.OnExit           := OnEditControltExit;

      ChannelNameEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('ChannelName');
      ChannelNameEdit.OnEnter            := OnEditControlEnter;
      ChannelNameEdit.OnExit             := OnEditControltExit;

      SummaryOutputChkBox.FieldProperty  := FAppModules.FieldProperties.FieldProperty('SummaryOutput');
      SummaryOutputChkBox.OnEnter        := OnEditControlEnter;
      SummaryOutputChkBox.OnClick        := OnSummaryOutputClick;

      UpstreamNodeCbx.FieldProperty      := FAppModules.FieldProperties.FieldProperty('UpNodeNumber');
      UpstreamNodeCbx.OnEnter            := OnEditControlEnter;
      UpstreamNodeCbx.OnExit             := OnEditControltExit;

      DownStreamNodeCbx.FieldProperty    := FAppModules.FieldProperties.FieldProperty('DownNodeNumber');
      DownStreamNodeCbx.OnEnter          := OnEditControlEnter;
      DownStreamNodeCbx.OnExit           := OnEditControltExit;

      UpStreamNodeEdit.FieldProperty      := FAppModules.FieldProperties.FieldProperty('UpNodeNumber');
      UpStreamNodeEdit.IsEnabled          := FALSE;
      UpStreamNodeEdit.OnEnter            := OnEditControlEnter;
      UpStreamNodeEdit.OnExit             := OnEditControltExit;

      DownStreamNodeEdit.FieldProperty    := FAppModules.FieldProperties.FieldProperty('DownNodeNumber');
      DownStreamNodeEdit.IsEnabled        := FALSE;
      DownStreamNodeEdit.OnEnter          := OnEditControlEnter;
      DownStreamNodeEdit.OnExit           := OnEditControltExit;


      PenaltyStructureEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyNumber');
      PenaltyStructureEdit.ReadOnly      := TRUE;
      PenaltyStructureEdit.Color         := clWindow;
      PenaltyStructureEdit.OnEnter       := OnEditControlEnter;
      PenaltyStructureEdit.OnExit        := OnEditControltExit;

      SelectPenaltyStructureBtn.FieldProperty := FAppModules.FieldProperties.FieldProperty('PenaltyNumber');
      SelectPenaltyStructureBtn.OnEnter  := OnEditControlEnter;
      SelectPenaltyStructureBtn.OnExit   := OnEditControltExit;
      SelectPenaltyStructureBtn.OnClick  := OnSelectPenaltyClick;

      SelectChannelBtn.OnClick           := OnSelectChannelClick;

      ChannelAreaCbx.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ChannelAreaID');
      ChannelAreaCbx.OnEnter             := OnEditControlEnter;
      ChannelAreaCbx.OnExit              := OnEditControltExit;

      FirmYieldAnalysisChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('FirmYieldCalc');
      FirmYieldAnalysisChkBox.OnEnter       := OnEditControlEnter;                        
      FirmYieldAnalysisChkBox.OnClick       := OnFirmYieldAnalysisClick;

      if GrowthChkBox <> nil then
      begin
        GrowthChkBox.FieldProperty            := FAppModules.FieldProperties.FieldProperty('Growth');
        GrowthChkBox.OnClick                  := DoGrowthChkBoxOnClick;
        GrowthChkBox.OnEnter                  := OnEditControlEnter;
        GrowthChkBox.Visible                  := True;

      end;

      if FlowOutputChkBox <> nil then
      begin
        FlowOutputChkBox.FieldProperty            := FAppModules.FieldProperties.FieldProperty('FlowOutput');
        FlowOutputChkBox.OnClick                  := DoFlowOutputChkBoxOnClick;
        FlowOutputChkBox.OnEnter                  := OnEditControlEnter;
        FlowOutputChkBox.Visible                  := True;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.CreateDialog;
const OPNAME = 'TGeneralFlowChannelValidator.CreateDialog';
begin
  try
    FPanel  := TGeneralFlowChannelDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.DestroyMemberObjects;
const OPNAME = 'TGeneralFlowChannelValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelValidator.Initialise: boolean;
const OPNAME = 'TGeneralFlowChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TGeneralFlowChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.ClearDataViewer;
const OPNAME = 'TGeneralFlowChannelValidator.ClearDataViewer';
var
  lpPanel     : TGeneralFlowChannelDialog;
begin
  inherited ClearDataViewer;
  try
    lpPanel := GeneralFlowChannelDialog;
    with lpPanel do
    begin
      ChannelNameEdit.Text        := '';
      ChannelNumberEdit.Text      := '-1';
      UpStreamNodeCbx.ItemIndex   := -1;
      DownStreamNodeCbx.ItemIndex := -1;
      UpStreamNodeEdit.Text       := '-1';
      DownStreamNodeEdit.Text     := '-1';
      PenaltyStructureEdit.Text   := '-1';
      SummaryOutputChkBox.Checked := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.PopulateDataViewer;
const OPNAME = 'TGeneralFlowChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtChanPropAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.RePopulateNodes;
const OPNAME = 'TGeneralFlowChannelValidator.RePopulateNodes';
var
  lChannel       : IGeneralFlowChannel;
  lReservoirList : IReservoirDataList;
  lIndexA        : integer;
  lReservoir     : IReservoirConfigurationData;
  LGrowthFactors : TGrowthFactors;
  LMinMaxChannelGrowthFactors : TMinMaxChannelGrowthFactors;
begin
  try
    GeneralFlowChannelDialog.UpStreamNodeCbx.Items.Clear;
    GeneralFlowChannelDialog.DownStreamNodeCbx.Items.Clear;
    if (FIdentifier >= 0) then
    begin

      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) then
      begin
        with GeneralFlowChannelDialog do
        begin
          if (FAppModules.StudyArea.ModelCode = CPlanning) then
          begin
            if lChannel.ChannelType = 8 then
            begin
              GrowthChkBox.Visible := True;
              LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
              if (LGrowthFactors <> nil) then
              begin
                LMinMaxChannelGrowthFactors := LGrowthFactors.CastMinMaxChannelGrowthFactorByMinMaxChannel(lChannel.ChannelNumber);
                if LMinMaxChannelGrowthFactors <> nil then
                  GrowthChkBox.Checked := True
                else
                  GrowthChkBox.Checked := False;
              end;
            end
            else
              GrowthChkBox.Visible := False;
          end;

          lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList;
          if (lReservoirList <> nil) then
          begin
            for lIndexA := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoir := lReservoirList.ReservoirOrNodeByIndex[lIndexA].
                              ReservoirConfigurationData;
              if (lReservoir.ReservoirIdentifier = 0) then
              begin
                if (lChannel.ChannelType <> 10) then // SpecifiedInflow
                  DownStreamNodeCbx.Items.AddObject('(0) ' + UpperCase(lChannel.SinkName),
                   TObject(lReservoir.ReservoirIdentifier));
                UpStreamNodeCbx.Items.AddObject('(0) ' + UpperCase(lChannel.SourceName),
                  TObject(lReservoir.ReservoirIdentifier));
              end
              else
              begin
                if lReservoir.NodeType = ntGroundWater then
                begin
                  if(lChannel.UpStreamNodeNumber <> lReservoir.ReservoirIdentifier) then
                      DownStreamNodeCbx.Items.AddObject
                        ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                         TObject(lReservoir.ReservoirIdentifier));
                end
                else
                begin
                  if not(lChannel.ChannelType in [31,35]) then
                  begin
                    case lReservoir.NodeType of
                      ntReservoir,
                      ntNodeWithInflow :
                        DownStreamNodeCbx.Items.AddObject
                          ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                           TObject(lReservoir.ReservoirIdentifier));
                      ntNodeWithoutInflow,ntDemandCentreNode :
                        if (lChannel.ChannelType <> 10) then
                          DownStreamNodeCbx.Items.AddObject
                            ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                             TObject(lReservoir.ReservoirIdentifier));
                      ntIrrigationNode :
                        if (lChannel.ChannelType in [4,12])  then
                          DownStreamNodeCbx.Items.AddObject
                            ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                             TObject(lReservoir.ReservoirIdentifier));
                      ntWetlandNode :
                        if (lChannel.ChannelType = 12)  then
                          DownStreamNodeCbx.Items.AddObject
                            ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                             TObject(lReservoir.ReservoirIdentifier));
                      ntMinePolutionControlDam :
                        if (lChannel.ChannelType = 12)  then
                          DownStreamNodeCbx.Items.AddObject
                            ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                             TObject(lReservoir.ReservoirIdentifier));
                    end;
                  end;
                end;

                if lReservoir.NodeType = ntGroundWater then
                begin
                  if(lChannel.DownStreamNodeNumber <> lReservoir.ReservoirIdentifier) then
                      UpStreamNodeCbx.Items.AddObject
                        ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                         TObject(lReservoir.ReservoirIdentifier));
                end
                else
                begin
                  if not(lChannel.ChannelType in [31,35]) then
                  begin
                     case lReservoir.NodeType of
                      ntReservoir,
                      ntNodeWithInflow,
                      ntNodeWithoutInflow,
                      ntDemandCentreNode :
                        UpStreamNodeCbx.Items.AddObject
                          ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                           TObject(lReservoir.ReservoirIdentifier));
                      ntIrrigationNode :
                        if (lChannel.ChannelType = 4)  then
                          UpStreamNodeCbx.Items.AddObject
                            ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                             TObject(lReservoir.ReservoirIdentifier));
                      ntWetlandNode :
                        UpStreamNodeCbx.Items.AddObject
                          ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                           TObject(lReservoir.ReservoirIdentifier));
                      ntMinePolutionControlDam :
                        if (lChannel.ChannelType = 12)  then
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
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.RePopulateDataViewer;
const OPNAME = 'TGeneralFlowChannelValidator.RePopulateDataViewer';
var
  lChannel       : IGeneralFlowChannel;
  lReservoirData : IReservoirData;
  lReservoir     : IReservoirConfigurationData;
  lPenalty       : IChannelPenalty;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  LIrrigationBlock  : IIrrigationBlock;
  LWetland          : IWetland;
  LMine             : IMine;
  LYMDemandCentre   : IYMDemandCentre;
  LGroundWater      : IGroundWater;
begin
  try
    if (FIdentifier >= 0) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) then
      begin
        {FMode := gfcvmChannel;
        case lChannel.ChannelType of
          14    : FMode := gfcvmIrrBlockUpstream;
          15    : FMode := gfcvmIrrBlockDownstream;
          16    : FMode := gfcvmWetlandUpstream;
          17    : FMode := gfcvmWetlandDownstream;
        end;}

        if(lChannel.ChannelType in [14,15,16,17]) then
        begin
          FUpStreamEnabled      := False;
          FDownStreamEnabled    := False;
          FSelectChannelEnabled := False;
        end;

        //Consumptive or reclaimation
        if(lChannel.ChannelType in [2,8,11,21]) then
        begin
          LYMDemandCentre  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.
                           YMDemandCentreByNodeNumber[lChannel.UpStreamNodeNumber];
          {if LYMDemandCentre <> nil then
            FMode := gfcvmDemandCentreConsumptiveUpstream;}
        end;

        with GeneralFlowChannelDialog do
        begin
          lFieldProperty := ChannelNumberEdit.FieldProperty;
          lKeyValues := lChannel.GetKeyValues(lFieldProperty.FieldName, '');
          ChannelNumberEdit.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          ChannelNumberEdit.SetFieldValue(IntToStr(lChannel.ChannelNumber));

          lFieldProperty := ChannelNameEdit.FieldProperty;
          ChannelNameEdit.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          ChannelNameEdit.SetFieldValue(lChannel.ChannelName);

          RePopulateNodes;

          lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lChannel.UpStreamNodeNumber];
          if (lReservoirData <> nil) then
          begin
            lFieldProperty := UpStreamNodeCbx.FieldProperty;
            UpStreamNodeCbx.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
            lReservoir := lReservoirData.ReservoirConfigurationData;
            UpstreamNodeCbx.SetFieldIndex
              (UpstreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
          end;
          lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lChannel.DownStreamNodeNumber];
          if (lReservoirData <> nil) then
          begin
            lFieldProperty := DownStreamNodeCbx.FieldProperty;
            DownStreamNodeCbx.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
            lReservoir := lReservoirData.ReservoirConfigurationData;
            DownstreamNodeCbx.SetFieldIndex
              (DownstreamNodeCbx.Items.IndexOfObject(TObject(lReservoir.ReservoirIdentifier)));
          end;

          UpStreamNodeCbx.Visible    := FUpStreamEnabled;
          UpStreamNodeEdit.Visible   := NOT FUpStreamEnabled;
          UpStreamNodeEdit.Text      := UpStreamNodeCbx.Text;
          DownStreamNodeCbx.Visible  := FDownStreamEnabled;
          DownStreamNodeEdit.Visible := NOT FDownStreamEnabled;
          DownStreamNodeEdit.Text    := DownStreamNodeCbx.Text;
          SelectChannelBtn.Visible   := FSelectChannelEnabled;

          if (lChannel.ChannelType = 12) then
          begin
            LYMDemandCentre := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.
                             YMDemandCentreByNodeNumber[lChannel.DownStreamNodeNumber];
            if LYMDemandCentre <> nil then
            begin
              FDownStreamEnabled          := False;
              DownStreamNodeEdit.Visible  := NOT FDownStreamEnabled;
              DownStreamNodeEdit.Text     := '('+IntToStr(LYMDemandCentre.NodeNumber)+') '+LYMDemandCentre.Name;
            end;
          end;

          RepopulateChannelAreas;

          lFieldProperty := PenaltyStructureEdit.FieldProperty;
          PenaltyStructureEdit.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          lPenalty := lChannel.ChannelPenalty;
          if (lPenalty <> nil) then
            PenaltyStructureEdit.SetFieldValue(lPenalty.ChannelPenaltyID)
          else
            PenaltyStructureEdit.SetFieldValue(0);

          lFieldProperty := SummaryOutputChkBox.FieldProperty;
          SummaryOutputChkBox.HasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, '') <> nil;
          SummaryOutputChkBox.Checked := (lChannel.SummaryOutputRequired = 'Y');

          if(FMode in [gfcvmIrrBlockUpstream,gfcvmWetlandUpstream])  then
          begin
            UpStreamNodeEdit.Text := '';
            lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lChannel.UpStreamNodeNumber];
            if (lReservoirData <> nil) then
              UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
          end;

          if(FMode in [gfcvmIrrBlockDownstream,gfcvmWetlandDownstream])  then
          begin
            DownStreamNodeEdit.Text := '';
            lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lChannel.DownStreamNodeNumber];
            if (lReservoirData <> nil) then
              DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
          end;

          if(FMode = gfcvmIrrBlockUpstream)  then
          begin
            DownStreamNodeEdit.Text := '';
            LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                 IrrigationBlockList.IrrigationBlockByBlockNodeNumber[lChannel.DownStreamNodeNumber];
            if(LIrrigationBlock <> nil) then
              DownStreamNodeEdit.Text := '(' + IntToStr(LIrrigationBlock.BlockNodeNumber) +
                                        ') ' + LIrrigationBlock.BlockName;
          end;

          if(FMode = gfcvmIrrBlockDownstream)  then
          begin
            UpStreamNodeEdit.Text := '';
            LIrrigationBlock  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                                 IrrigationBlockList.IrrigationBlockByBlockNodeNumber[lChannel.UpStreamNodeNumber];
            if(LIrrigationBlock <> nil) then
              UpStreamNodeEdit.Text := '(' + IntToStr(LIrrigationBlock.BlockNodeNumber) +
                                        ') ' + LIrrigationBlock.BlockName;
          end;

         if(FMode = gfcvmMineToRiver)  then
          begin
            UpStreamNodeEdit.Text := '';
            LMine  := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastMineList.MineByNodeNumber[lChannel.UpStreamNodeNumber];
            if(LMine <> nil) then
              UpStreamNodeEdit.Text := '(' + IntToStr(LMine.NodeNumber) +
                                        ') ' + LMine.MineName;
          end;

          if(FMode = gfcvmMineToPCD)  then
          begin
            UpStreamNodeEdit.Text := '';
             if(lChannel <> nil) then
            begin
              UpStreamNodeEdit.Text := '(' + IntToStr(lChannel.UpStreamNodeNumber) +
                                        ') ' + lChannel.SourceName;

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lChannel.DownStreamNodeNumber];
              if (lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;

          if(FMode = gfcvmMineToUndeground)  then
          begin
            UpStreamNodeEdit.Text := '';
            LMine  := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastMineList.MineByNodeNumber[lChannel.UpStreamNodeNumber];
            if(LMine <> nil) then
            begin
              UpStreamNodeEdit.Text := '(' + IntToStr(LMine.NodeNumber) +
                                        ') ' + LMine.MineName;

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lChannel.DownStreamNodeNumber];
              if (lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;

          if(FMode = gfcvmWetlandUpstream) then
          begin
            DownStreamNodeEdit.Text := '';
            LWetland  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.
                         WetlandByNodeNumber[lChannel.DownStreamNodeNumber];
            if(LWetland <> nil) then
              DownStreamNodeEdit.Text := '(' + IntToStr(LWetland.NodeNumber) +
                                        ') ' + LWetland.Name;
          end;

          if(FMode = gfcvmWetlandDownstream) then
          begin
            UpStreamNodeEdit.Text := '';
            LWetland  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.WetlandList.
                         WetlandByNodeNumber[lChannel.UpStreamNodeNumber];
            if(LWetland <> nil) then
              UpStreamNodeEdit.Text := '(' + IntToStr(LWetland.NodeNumber) +
                                        ') ' + LWetland.Name;
          end;

          if(FMode in [gfcvmDemandCentreConsumptiveUpstream, gfcvmDemandCentreReclaimationUpstream])  then
          begin
            LYMDemandCentre  := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.YMDemandCentreList.
                                YMDemandCentreByNodeNumber[lChannel.UpStreamNodeNumber];
            UpStreamNodeEdit.Text := '';
            if(LYMDemandCentre <> nil) then
              UpStreamNodeEdit.Text := '(' + IntToStr(LYMDemandCentre.NodeNumber) +
                                        ') ' + LYMDemandCentre.Name;
          end;

          if(FMode = gfcvmGroundWaterAquiferInflowChannel)  then
          begin
            UpStreamNodeEdit.Text := '';
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByNodeNumber[lChannel.DownStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := IntToStr(lChannel.UpStreamNodeNumber);

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.AquiferNodeNr];
              if (lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;

          if(FMode = gfcvmOutflowToDownstreamAquiferChannel)  then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByNodeNumber[lChannel.UpStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.AquiferNodeNr];
              if (lReservoirData <> nil) then
                UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;

              DownStreamNodeEdit.Text := IntToStr(lChannel.DownStreamNodeNumber);
            end;
          end;

          if(FMode = gfcvmInflowFromUpstreamAquiferChannel) then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                            CastGroundWaterList.GroundWaterByNodeNumber[lChannel.DownStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                                ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.AquiferNodeNr];
              if(lReservoirData <> nil) then
                UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                                ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.AquiferNodeNr];
              if(lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                           ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;

          if(FMode = gfcvmGroundWaterBaseflowChannel)  then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByBaseFlowNumber[lChannel.DownStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.AquiferNodeNr];
              if (lReservoirData <> nil) then
                UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.BaseFlowNodeNr];
              if (lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;

          if(FMode = gfcvmAbtstractionFromAquiferChannel)  then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByAbstractionNodeNumber[lChannel.DownStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.AquiferNodeNr];
              if (lReservoirData <> nil) then
                UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.AbstractionNodeNr];
              if (lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;

          if(FMode = gfcvmAbtstractionGroundWaterBaseflowChannel)  then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByAbstractionNodeNumber[lChannel.DownStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.BaseFlowNodeNr];
              if (lReservoirData <> nil) then
                UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.AbstractionNodeNr];
              if (lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;

          if(FMode = gfcvmExcessInterflowChannel)  then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByCollectionNodeNumber[lChannel.DownStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[lChannel.UpStreamNodeNumber];
              if (lReservoirData <> nil) then
                UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             ReservoirList.ReservoirOrNodeByIdentifier[lChannel.DownStreamNodeNumber];
              if (lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;

          if(FMode = gfcvmSurfaceRunoffChannel)  then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByCollectionNodeNumber[lChannel.DownStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              UpStreamNodeEdit.Text := IntToStr(lChannel.UpStreamNodeNumber);

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.CollectionNodeNr];
              if (lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;
          
          if(FMode = gfcvmGroundWaterBaseFlowRemainderChannel)  then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByBaseFlowNumber[lChannel.UpStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.BaseFlowNodeNr];
              if (lReservoirData <> nil) then
                UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;

              DownStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                             ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.CollectionNodeNr];
              if (lReservoirData <> nil) then
                DownStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;
            end;
          end;

          if(FMode = gfcvmGroundWaterAbstractionChannel)  then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByAbstractionNodeNumber[lChannel.UpStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.AbstractionNodeNr];
              if (lReservoirData <> nil) then
                UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;

              DownStreamNodeEdit.Text := IntToStr(lChannel.DownStreamNodeNumber);
            end;
          end;

          if(FMode = gfcvmOutflowToNetworkChannel)  then
          begin
            LGroundWater := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.
                                 CastGroundWaterList.GroundWaterByCollectionNodeNumber[lChannel.UpStreamNodeNumber];
            if(LGroundWater <> nil) then
            begin
              UpStreamNodeEdit.Text := '';
              lReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData.
                              ReservoirList.ReservoirOrNodeByIdentifier[LGroundWater.CollectionNodeNr];
              if (lReservoirData <> nil) then
                UpStreamNodeEdit.Text := '(' + IntToStr(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier) +
                                        ') ' + lReservoirData.ReservoirConfigurationData.ReservoirName;

              DownStreamNodeEdit.Text := IntToStr(lChannel.DownStreamNodeNumber);
            end;
          end;
          PopulateFirmYieldAnalysis;
        end;
      end;
    end;

    if (FMode = gfcvmDemandCentreReclaimationUpstream) then
      if(FPanel <> nil) and (FPanel.Parent <> nil) and (FPanel.Parent Is TTabSheet) then
        TTabSheet(FPanel.Parent).TabVisible := (FIdentifier > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelValidator.SaveState: boolean;
const OPNAME = 'TGeneralFlowChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelValidator.GeneralFlowChannelDialog:TGeneralFlowChannelDialog;
const OPNAME = 'TGeneralFlowChannelValidator.GeneralFlowChannelDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TGeneralFlowChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TGeneralFlowChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
 
    if (AFieldName = 'DemandCentreConsumptiveChannelNumber') then
    begin
      if (AContext = sdccEdit) and ((AOldValue = IntToStr(FIdentifier)) or (FIdentifier = -1)) then
      begin
        if (FMode = gfcvmDemandCentreConsumptiveUpstream) then
        begin
          FIdentifier := StrToInt(ANewValue);
          RePopulateDataViewer;
        end;
      end;
    end else
    if (AFieldName = 'DemandCentreReclaimationChannelNumber') then
    begin
      if (AContext = sdccEdit) then
      begin
        if(FMode = gfcvmDemandCentreReclaimationUpstream) and ((AOldValue = IntToStr(FIdentifier)) or (FIdentifier = -1)) then
        begin
          FIdentifier := StrToInt(ANewValue);
          ClearDataViewer;
          RePopulateDataViewer;
        end;
      end;
    end else
    if (AFieldName = 'Penalty') then
      DoContextValidation(dvtChanPropPenalty)
    else if (AFieldName = 'ChannelName') then
      RePopulateDataViewer
    else if (AFieldName = 'PenaltyNumber') then
      RePopulateDataViewer
    else if (AFieldName = 'DownNodeNumber') then
      RePopulateDataViewer
    else if (AFieldName = 'UpNodeNumber') then
      RePopulateDataViewer
    else if (AFieldName = 'SummaryOutput') then
      RePopulateDataViewer;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TGeneralFlowChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.OnSelectPenaltyClick(Sender: TObject);
const OPNAME = 'TGeneralFlowChannelValidator.OnSelectPenaltyClick';
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TChannelPenaltyValidator;
  LPenaltyPanelList : TList;
  LSelectedPenalty,
  LIndex            : integer;
  LActivePanel      : TChannelPenaltyPanel;
begin
  try
    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.LanguageHasChanged;

      LDialogValidator := TChannelPenaltyValidator.Create(LForm,FAppModules);
      FChannelPenaltyValidator := LDialogValidator;
      try
        LForm.AddModelDataPanel(LDialogValidator.Panel);
        LDialogValidator.Initialise;
        LDialogValidator.ViewMode := vmSelect;
        LDialogValidator.ChannelNumber := FIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        lForm.OnShow := ShowChannelPenaltyDialog;

        LPenaltyPanelList := TChannelPenaltyDialog(LDialogValidator.Panel).PenaltyPanels;
        for LIndex := 0 to LPenaltyPanelList.Count - 1 do
        begin
          LActivePanel := LPenaltyPanelList.Items[LIndex];
          LActivePanel.DescriptionEdit.OnDblClick  := OnEditControlDoubleClick;
          LActivePanel.ArcPenaltiesGrid.OnDblClick := OnEditControlDoubleClick;
        end;

        LForm.ShowModal;
        if (LForm.ModalResult = mrOk) and (TChannelPenaltyDialog(LDialogValidator.Panel).ActivePanel <> nil)then
        begin
          LSelectedPenalty := StrToInt(TChannelPenaltyDialog(LDialogValidator.Panel).ActivePanel.IDLabel.Caption);
          if(GeneralFlowChannelDialog.PenaltyStructureEdit.Text <> IntToStr(LSelectedPenalty)) then
          begin
            GeneralFlowChannelDialog.PenaltyStructureEdit.Text := IntToStr(LSelectedPenalty);
            GeneralFlowChannelDialog.PenaltyStructureEdit.OnExit(GeneralFlowChannelDialog.PenaltyStructureEdit);
          end;
        end;
      finally
        FChannelPenaltyValidator := nil;
        LDialogValidator.Free;
      end;
    finally
      LForm.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.ShowChannelPenaltyDialog(ASender: TObject);
const OPNAME = 'TGeneralFlowChannelValidator.ShowChannelPenaltyDialog';
begin
  try
    if (FChannelPenaltyValidator <> nil) then
      FChannelPenaltyValidator.ChannelPenaltyDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.OnSelectChannelClick(Sender: TObject);
const OPNAME = 'TGeneralFlowChannelValidator.OnSelectChannelClick';
{
var
  LForm            : TYieldModelDataGUIForm;
  LDialogValidator : TSelectChannelValidator;
  lSelectedNumber  : integer;
  lOldChannel      : TGeneralFlowChannel;
  lNewChannel      : TGeneralFlowChannel;
  lIndex           : integer;}
begin
  try
{    LForm   := TYieldModelDataGUIForm.CreateWithoutDFM(nil,FAppModules);
    try
      LForm.Initialise;
      LForm.LanguageHasChanged;

      LDialogValidator := TSelectChannelValidator.Create(LForm,FAppModules);
      FSelectChannelValidator := LDialogValidator;
      try
        LForm.AddModelDataPanel(LDialogValidator.Panel);
        LDialogValidator.Initialise;
        LDialogValidator.ChannelNumber := FIdentifier;
        LDialogValidator.PopulateDataViewer;
        LDialogValidator.LanguageHasChanged;
        lForm.OnShow := ShowSelectChannelDialog;

        LForm.ShowModal;
        if (LForm.ModalResult = mrOk) then
        begin
          with LDialogValidator.SelectChannelDialog do
          begin
            lIndex := ChannelsListBox.ItemIndex;
            if (lIndex >= 0) then
            begin
              LSelectedNumber := Integer(ChannelsListBox.Items.Objects[lIndex]);
              if (FIdentifier <> LSelectedNumber) then
              begin
                lOldChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                                 CastNetworkElementData.CastChannelList.CastChannelByChannelNumber[FIdentifier];
                lNewChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                                 CastNetworkElementData.CastChannelList.CastChannelByChannelNumber[LSelectedNumber];
                if (lOldChannel <> nil) then
                begin
                  if (lOldChannel.DiversionFeature <> nil) then
                  begin

                  end;
                  FIdentifier := LSelectedNumber;
                  RePopulateDataViewer;
                end;
              end;
            end;
          end;
        end;
      finally
        FSelectChannelValidator := nil;
        LDialogValidator.Free;
      end;
    finally
      LForm.Free;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.OnFirmYieldAnalysisClick(Sender: TObject);
const OPNAME = 'TGeneralFlowChannelValidator.OnFirmYieldAnalysisClick';
begin
  try
    if(GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.HasValueChanged) then
      UpdateChannelFirmYieldAnalysis;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.UpdateChannelFirmYieldAnalysis;
const OPNAME = 'TGeneralFlowChannelValidator.UpdateChannelFirmYieldAnalysis';
var
  lChannel  : IGeneralFlowChannel;
  lOldValue : string;
  lNewValue : string;
  lMessage  : string;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil ) then
    begin
      with GeneralFlowChannelDialog do
      begin
        lOldValue := UpperCase(Trim(lChannel.RequiresFirmYieldAnalysis));
        if FirmYieldAnalysisChkBox.Checked then
          lNewValue := 'Y'
        else
          lNewValue := 'N';
        if (lOldValue <> lNewvalue) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              FirmYieldAnalysisChkBox.FieldProperty.FieldName,
              lNewValue,lMessage)) then
          begin
            lChannel.RequiresFirmYieldAnalysis := lNewValue;
            FirmYieldAnalysisChkBox.Checked := (lChannel.RequiresFirmYieldAnalysis = 'Y');
          end
          else
            FirmYieldAnalysisChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.PopulateFirmYieldAnalysis;
const OPNAME = 'TGeneralFlowChannelValidator.PopulateFirmYieldAnalysis';
var
  lFirmYield  : boolean;
  lChannel    : IGeneralFlowChannel;
  lConfigData : IRunConfigurationData;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Visible := False;
      if (FAppModules.Model.ModelName = CYield) then
      begin
        if(lChannel.ChannelType in [ctMinimumFlowChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel,ctDemandCentreReturnFlowChannel]) then
        begin
          GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Visible := True;
          GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Checked := (lChannel.RequiresFirmYieldAnalysis = 'Y');
          GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Enabled := (lChannel.SummaryOutputRequired = 'Y');
        end;
      end
      else if (FAppModules.Model.ModelName = CPlanning) then
      begin
        lConfigData := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
        if (lConfigData <> nil) then
        begin
          lFirmYield := lConfigData.CreatePlotFile;
          GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Visible := True;
          GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Enabled := (lChannel.SummaryOutputRequired = 'Y') and lFirmYield;
          if GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Enabled then
            GeneralFlowChannelDialog.FirmYieldAnalysisChkBox.Checked := (lChannel.RequiresFirmYieldAnalysis = 'Y');
          GeneralFlowChannelDialog.FlowOutputChkBox.Enabled := ((lChannel.SummaryOutputRequired = 'Y') and lConfigData.CreatePlotFile);
          if GeneralFlowChannelDialog.FlowOutputChkBox.Enabled then
            GeneralFlowChannelDialog.FlowOutputChkBox.Checked := (lChannel.FlowOutput = 'Y');
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TGeneralFlowChannelValidator.ShowSelectChannelDialog(ASender: TObject);
const OPNAME = 'TGeneralFlowChannelValidator.ShowSelectChannelDialog';
begin
  try
    if (FSelectChannelValidator <> nil) then
      FSelectChannelValidator.SelectChannelDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TGeneralFlowChannelValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TGeneralFlowChannelValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with GeneralFlowChannelDialog do
    begin
      if ((Sender = ChannelNameEdit) AND
          (ChannelNameEdit.HasValueChanged)) then
        UpdateChannelName
      else
      if ((Sender = PenaltyStructureEdit) AND
          (PenaltyStructureEdit.HasValueChanged)) then
        UpdateChannelPenaltyStructure
      else
      if ((Sender = UpstreamNodeCbx) AND
          (UpstreamNodeCbx.HasValueChanged)) then
        UpdateChannelUpstreamNode
      else
      if ((Sender = DownstreamNodeCbx) AND
          (DownstreamNodeCbx.HasValueChanged)) then
        UpdateChannelDownstreamNode
      else
      if ((Sender = ChannelAreaCbx) AND (ChannelAreaCbx.HasValueChanged)) then
        UpdateChannelArea;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.OnEditControlDoubleClick(ASender: TObject);
const OPNAME = 'TGeneralFlowChannelValidator.OnEditControlDoubleClick';
var
  LSelectedPenalty : integer;
begin
  try
    LSelectedPenalty := StrToInt(TChannelPenaltyDialog(FChannelPenaltyValidator.Panel).ActivePanel.IDLabel.Caption);
    if(GeneralFlowChannelDialog.PenaltyStructureEdit.Text <> IntToStr(LSelectedPenalty)) then
    begin
      GeneralFlowChannelDialog.PenaltyStructureEdit.Text := IntToStr(LSelectedPenalty);
      GeneralFlowChannelDialog.PenaltyStructureEdit.OnExit(GeneralFlowChannelDialog.PenaltyStructureEdit);
    end;
    TYieldModelDataGUIForm(FChannelPenaltyValidator.Panel.Parent).ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.UpdateChannelName;
const OPNAME = 'TGeneralFlowChannelValidator.UpdateChannelName';
var
  lChannel : IGeneralFlowChannel;
  lMessage : string;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            ChannelNameEdit.FieldProperty.FieldName,
            ChannelNameEdit.Text,lMessage)) then
        begin
          ChannelNameEdit.FieldValidationError := lMessage;
          lChannel.ChannelName := Trim(ChannelNameEdit.Text);
          ChannelNameEdit.SetFieldValue(lChannel.ChannelName);
          DoContextValidation(dvtChanPropName);
        end
        else
          ChannelNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.DoGrowthChkBoxOnClick(Sender : TObject);
const OPNAME = 'TGeneralFlowChannelValidator.DoGrowthChkBoxOnClick';
var
  LChannel : IGeneralFlowChannel;
  LGrowthFactors : TGrowthFactors;
  LMinMaxChannelGrowthFactors : IMinMaxChannelGrowthFactors;
  LTempStr : TStringList;
  LNumberOfYears,
  LCount,
  LArc : integer;
  LRunConfig : IRunConfigurationData;
  LPenaltyStructure : IChannelPenalty;
begin
  try
    LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];

    LGrowthFactors := TPlanningModelDataObject(FAppModules.Model.ModelData).CastGrowthFactors;
    LRunConfig := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData;
    if (LChannel <> nil) and (LRunConfig <> nil) then
    begin
      if LGrowthFactors <> nil then
      begin
        if GeneralFlowChannelDialog.GrowthChkBox.Checked then
        begin
          Growth := True;
          LMinMaxChannelGrowthFactors := LGrowthFactors.MinMaxChannelGrowthFactorsByMinMaxChannel[LChannel.ChannelNumber];
          if LMinMaxChannelGrowthFactors = nil then
          begin
            LPenaltyStructure := LChannel.ChannelPenalty;
            if LPenaltyStructure <> nil then
             begin
              for LArc := 1 to  LPenaltyStructure.ChannelPenaltyArcCount do
              begin
                LMinMaxChannelGrowthFactors :=  LGrowthFactors.AddMinMaxChannelGrowthFactor(LChannel.ChannelNumber);
                if LMinMaxChannelGrowthFactors <> nil then
                begin
                  LTempStr := TStringList.Create;
                  try
                    LNumberOfYears := LGrowthFactors.NumberOfYears;
                    for LCount := 0 to LNumberOfYears do
                      LTempStr.Add('0.0000');
                    LMinMaxChannelGrowthFactors.GrowthFactors := LTempStr.CommaText;
                    LMinMaxChannelGrowthFactors.ArcNumber := LArc;
                  finally
                      LTempStr.Free;
                  end;
                end;
              end;
            end;
          end;
        end
        else
        if not (GeneralFlowChannelDialog.GrowthChkBox.Checked) then
        begin
          Growth := False;
          LMinMaxChannelGrowthFactors :=   LGrowthFactors.MinMaxChannelGrowthFactorsByMinMaxChannel[LChannel.ChannelNumber];
          if LMinMaxChannelGrowthFactors <> nil then
            LGrowthFactors.RemoveMinMaxChannelGrowthFactor(LChannel.ChannelNumber);
        end;
      end;
    end;

   // FAppModules.Model.StudyHasChanged;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TGeneralFlowChannelValidator.UpdateChannelFlowOutput;
const OPNAME = 'TGeneralFlowChannelValidator.UpdateChannelIncludeSummary';
var
  lChannel    : IGeneralFlowChannel;
  lOldInclude : string;
  lNewInclude : string;
  lMessage    : string;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        lOldInclude := UpperCase(Trim(lChannel.FlowOutput));
        if FlowOutputChkBox.Checked then
          lNewInclude := 'Y'
        else
          lNewInclude := 'N';
        if (lOldInclude <> lNewInclude) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              FlowOutputChkBox.FieldProperty.FieldName,
              lNewInclude,lMessage)) then
          begin
            lChannel.FlowOutput := lNewInclude;
            PopulateFirmYieldAnalysis;
          end
          else
            FlowOutputChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.DoFlowOutputChkBoxOnClick(Sender : TObject);
const OPNAME = 'TGeneralFlowChannelValidator.DoFlowOutputChkBoxOnClick';
begin
  try
    if(GeneralFlowChannelDialog.FlowOutputChkBox.HasValueChanged) then
      UpdateChannelFlowOutput;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TGeneralFlowChannelValidator.UpdateChannelPenaltyStructure;
const OPNAME = 'TGeneralFlowChannelValidator.UpdateChannelPenaltyStructure';
var
  lPenaltyTypeList  : IChannelPenaltyList;
  lPenaltyStructure : IChannelPenalty;
  lPenaltyID        : integer;
  lChannel          : IGeneralFlowChannel;
  lMessage          : string;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            PenaltyStructureEdit.FieldProperty.FieldName,
            PenaltyStructureEdit.Text,lMessage)) then
        begin
          PenaltyStructureEdit.FieldValidationError := lMessage;
          lPenaltyID       := StrToInt(Trim(PenaltyStructureEdit.Text));
          lPenaltyTypeList := TYieldModelDataObject(FAppModules.Model.ModelData).
                                NetworkElementData.ChannelPenaltyList;
          if (lPenaltyTypeList <> nil) then
          begin
            lPenaltyStructure := lPenaltyTypeList.ChannelPenaltyByIdentifier[lPenaltyID];
            lChannel.ChannelPenalty := lPenaltyStructure;
            PenaltyStructureEdit.SetFieldValue(IntToStr(lChannel.ChannelPenalty.ChannelPenaltyID));
            DoContextValidation(dvtChanPropPenalty);
          end;
        end
        else
          PenaltyStructureEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.UpdateChannelUpstreamNode;
const OPNAME = 'TGeneralFlowChannelValidator.UpdateChannelUpstreamNode';
var
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lMessage       : string;
  lReservoirList : IReservoirDataList;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir   := nil;
        lReservoirNr := -1;
        if (UpstreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(UpstreamNodeCbx.Items.Objects[UpstreamNodeCbx.ItemIndex]);
          lReservoir := lReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr];
        end;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            UpstreamNodeCbx.FieldProperty.FieldName,
            IntToStr(lReservoirNr),lMessage)) then
        begin
          lChannel.UpStreamNodeNumber := lReservoirNr;
          lReservoir := lChannel.UpStreamNode;
          UpstreamNodeCbx.SetFieldIndex
            (UpstreamNodeCbx.Items.IndexOfObject
              (TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier)));
          DoContextValidation(dvtChanPropUpstreamNode);
        end
        else
          UpstreamNodeCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.UpdateChannelDownstreamNode;
const OPNAME = 'TGeneralFlowChannelValidator.UpdateChannelDownstreamNode';
var
  lReservoir     : IReservoirData;
  lReservoirNr   : integer;
  lChannelList   : IChannelList;
  lChannel       : IGeneralFlowChannel;
  lMessage       : string;
  lReservoirList : IReservoirDataList;
begin
  try
    lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ChannelList;
    lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
        lReservoir   := nil;
        lReservoirNr := -1;
        if (DownstreamNodeCbx.ItemIndex >= 0) then
        begin
          lReservoirNr := Integer(DownstreamNodeCbx.Items.Objects[DownstreamNodeCbx.ItemIndex]);
          lReservoir := lReservoirList.ReservoirOrNodeByIdentifier[lReservoirNr];
        end;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            DownstreamNodeCbx.FieldProperty.FieldName,
            IntToStr(lReservoirNr), lMessage)) then
        begin
          lChannel.DownstreamNodeNumber := lReservoirNr;
          lReservoir := lChannel.DownStreamNode;
          DownstreamNodeCbx.SetFieldIndex
            (DownstreamNodeCbx.Items.IndexOfObject
              (TObject(lReservoir.ReservoirConfigurationData.ReservoirIdentifier)));
          DoContextValidation(dvtChanPropDownstreamNode);
        end
        else
          DownstreamNodeCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.UpdateChannelIncludeSummary;
const OPNAME = 'TGeneralFlowChannelValidator.UpdateChannelIncludeSummary';
var
  lChannel    : IGeneralFlowChannel;
  lOldInclude : string;
  lNewInclude : string;
  lMessage    : string;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                  NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        lOldInclude := UpperCase(Trim(lChannel.SummaryOutputRequired));
        if SummaryOutputChkBox.Checked then
          lNewInclude := 'Y'
        else
          lNewInclude := 'N';
        if (lOldInclude <> lNewInclude) then
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              SummaryOutputChkBox.FieldProperty.FieldName,
              lNewInclude,lMessage)) then
          begin
            lChannel.SummaryOutputRequired := lNewInclude;
            SummaryOutputChkBox.Checked := (lChannel.SummaryOutputRequired = 'Y');
            if not SummaryOutputChkBox.Checked and (lChannel.RequiresFirmYieldAnalysis = 'Y') then
               lChannel.RequiresFirmYieldAnalysis := 'N';
            PopulateFirmYieldAnalysis;
          end
          else
            SummaryOutputChkBox.ValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.DoContextValidation(AValidationType : TDialogValidationType);
const OPNAME = 'TGeneralFlowChannelValidator.DoContextValidation';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) then
      begin
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep2, dvtChanPropName]) then
          ValidateChannelName(lChannel);
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep2, dvtChanPropPenalty]) then
          ValidateChannelPenalty(lChannel);
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep1, dvtChanPropUpstreamNode,
                                dvtChanPropDownstreamNode]) then
          ValidateUpAndDownstreamNodes(lChannelList, lChannel);
        if (AValidationType in [dvtChanPropAll, dvtChanPropWizardStep2, dvtChanPropNumber]) then
          ValidateChannelNumber(lChannel);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TGeneralFlowChannelValidator.DetermineWizardStatus';
var
  lChannel     : IGeneralFlowChannel;
  lChannelList : IChannelList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
      lChannel     := lChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) then
      begin
        case ASequence of
          1 :
            begin
              DoContextValidation(dvtChanPropWizardStep1);
              if ((lChannel.UpStreamNodeNumber <> 0) OR (lChannel.DownStreamNodeNumber <> 0)) then
              begin
                Result := 1;
                if (FAllErrorMessages.Count = 0) then
                  Result := 2;
              end;
            end;
          2 :
            begin
              DoContextValidation(dvtChanPropWizardStep2);
              if (lChannel.ChannelPenaltyNumber <> 0) then
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

procedure TGeneralFlowChannelValidator.ValidateChannelNumber(AChannel: IGeneralFlowChannel);
const OPNAME = 'TGeneralFlowChannelValidator.ValidateChannelNumber';
begin
  try
    with GeneralFlowChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannel.Validate(FErrorMessage, 'ChannelNumber')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      ChannelNumberEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.ValidateChannelName (AChannel : IGeneralFlowChannel);
const OPNAME = 'TGeneralFlowChannelValidator.ValidateChannelName';
begin
  try
    with GeneralFlowChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannel.Validate(FErrorMessage, 'ChannelName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      ChannelNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.ValidateChannelPenalty (AChannel : IGeneralFlowChannel);
const OPNAME = 'TGeneralFlowChannelValidator.ValidateChannelPenalty';
begin
  try
    with GeneralFlowChannelDialog do
    begin
      FErrorMessage := '';
      if (NOT AChannel.Validate(FErrorMessage, 'ChannelPenalty')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      PenaltyStructureEdit.FieldValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.ValidateUpAndDownstreamNodes
                                        (AChannelList : IChannelList;
                                         AChannel     : IGeneralFlowChannel);
const OPNAME = 'TGeneralFlowChannelValidator.ValidateUpAndDownstreamNodes';
begin
  try
    with GeneralFlowChannelDialog do
    begin
      FErrorMessage := '';
      if (AChannel.Validate(FErrorMessage, 'UpDownNotSameNode')) then
      begin
        FErrorMessage := '';
        if (AChannel.Validate(FErrorMessage, 'UpNodeNumber')) then
        begin
          if (AChannel.UpStreamNodeNumber <> 0) then
          begin
            UpstreamNodeCbx.InValidationError := FALSE;
            UpstreamNodeCbx.ShowErrorState(FALSE);
          end
          else
          begin
            if (AChannelList.Validate(FErrorMessage, 'MaximumZeroUpstreamNode')) then
            begin
              UpstreamNodeCbx.InValidationError := FALSE;
              UpstreamNodeCbx.ShowErrorState(FALSE);
            end
            else
            begin
              UpstreamNodeCbx.InValidationError := TRUE;
              UpstreamNodeCbx.ValidationError := FErrorMessage;
              UpstreamNodeCbx.ShowErrorState(TRUE);
              FAllErrorMessages.Add(Trim(FErrorMessage));
            end;
          end
        end
        else
        begin
          UpstreamNodeCbx.InValidationError := TRUE;
          UpstreamNodeCbx.ValidationError := FErrorMessage;
          UpstreamNodeCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;

        FErrorMessage := '';
        if (AChannel.Validate(FErrorMessage, 'DownNodeNumber')) then
        begin
          if (AChannel.DownStreamNodeNumber <> 0) then
          begin
            DownstreamNodeCbx.InValidationError := FALSE;
            DownstreamNodeCbx.ShowErrorState(FALSE);
          end
          else
          begin
            if (AChannelList.Validate(FErrorMessage, 'MaximumZeroDownstreamNode')) then
            begin
              DownstreamNodeCbx.InValidationError := FALSE;
              DownstreamNodeCbx.ShowErrorState(FALSE);
            end
            else
            begin
              DownstreamNodeCbx.InValidationError := TRUE;
              DownstreamNodeCbx.ValidationError := FErrorMessage;
              DownstreamNodeCbx.ShowErrorState(TRUE);
              FAllErrorMessages.Add(Trim(FErrorMessage));
            end;
          end
        end
        else
        begin
          DownstreamNodeCbx.InValidationError := TRUE;
          DownstreamNodeCbx.ValidationError := FErrorMessage;
          DownstreamNodeCbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end
      else
      begin
        UpstreamNodeCbx.InValidationError   := TRUE;
        UpstreamNodeCbx.ValidationError     := FErrorMessage;
        DownstreamNodeCbx.InValidationError := TRUE;
        DownstreamNodeCbx.ValidationError   := FErrorMessage;
        UpstreamNodeCbx.ShowErrorState(TRUE);
        DownstreamNodeCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGeneralFlowChannelValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TGeneralFlowChannelValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lChannel       : IGeneralFlowChannel;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FIdentifier <> 0)) then
    begin
      lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
      if (lChannel <> nil) then
      begin
        with GeneralFlowChannelDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = ChannelNameEdit) then
            lFieldProperty := ChannelNameEdit.FieldProperty
          else
          if (FActiveControl = ChannelNumberEdit) then
            lFieldProperty := ChannelNumberEdit.FieldProperty
          else
          if (FActiveControl = UpStreamNodeCbx) then
            lFieldProperty := UpStreamNodeCbx.FieldProperty
          else
          if (FActiveControl = DownStreamNodeCbx) then
            lFieldProperty := DownStreamNodeCbx.FieldProperty
          else
          if (FActiveControl = PenaltyStructureEdit) then
            lFieldProperty := PenaltyStructureEdit.FieldProperty
          else
          if (FActiveControl = SummaryOutputChkBox) then
            lFieldProperty := SummaryOutputChkBox.FieldProperty
          else
          if (FActiveControl = ChannelAreaCbx) then
            lFieldProperty := ChannelAreaCbx.FieldProperty;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lChannel.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
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

procedure TGeneralFlowChannelValidator.RepopulateChannelAreas;
const OPNAME = 'TGeneralFlowChannelValidator.RepopulateChannelAreas';
var
  LAreas           : TStringList;
  LIndex           : integer;
  lChannelAreaName : string;
  lChannelAreaList : IChannelAreaList;
  lChannelAreaData : IChannelArea;
  lChannel         : IGeneralFlowChannel;
  lFieldIndex      : string;
  lFieldProperty   : TAbstractFieldProperty;
  lKeyValues       : string;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      LAreas := TStringList.Create;
      try
        LAreas.Sorted := True;
        LAreas.Duplicates := dupIgnore;
        for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.ChannelAreaList.AreaCount -1 do
        begin
          LAreas.Add(TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.ChannelAreaList.ChannelAreaByIndex(LIndex).AreaName)
        end;

        GeneralFlowChannelDialog.ChannelAreaCbx.Items.Assign(LAreas);
        if (lChannel.ChannelArea <> 0) then
        begin
          lChannelAreaName := '';
          lChannelAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkFeaturesData.ChannelAreaList;
          lChannelAreaData := lChannelAreaList.ChannelAreaByID(lChannel.ChannelArea);

          lFieldIndex    := '';
          lFieldProperty := GeneralFlowChannelDialog.ChannelAreaCbx.FieldProperty;
          lKeyValues     := lChannel.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          GeneralFlowChannelDialog.ChannelAreaCbx.HasMetaData :=
            FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;

          if (lChannelAreaData <> nil) then
          begin
            lChannelAreaName := lChannelAreaData.AreaName;
            GeneralFlowChannelDialog.ChannelAreaCbx.ItemIndex :=
            GeneralFlowChannelDialog.ChannelAreaCbx.Items.IndexOf(lChannelAreaName);
          end;
        end;
      finally
        LAreas.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.UpdateChannelArea;
const OPNAME = 'TGeneralFlowChannelValidator.UpdateChannelArea';
var
  lChannel         : IGeneralFlowChannel;
  lChannelArea     : IChannelArea;
  lChannelAreaList : IChannelAreaList;
  lMessage         : string;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      with GeneralFlowChannelDialog do
      begin
        lChannelAreaList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                            .ChannelAreaList;
        if (lChannelAreaList <> nil) then
        begin
          lChannelArea := lChannelAreaList.ChannelAreaByName(ChannelAreaCbx.Text);
          if(lChannelArea <> nil) AND
          (lChannelArea.AreaID <> lChannel.ChannelArea) then
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty(
               ChannelAreaCbx.FieldProperty.FieldName,
               IntToStr(lChannelArea.AreaID),lMessage)) then
            begin
              ChannelAreaCbx.ValidationError := lMessage;
              lChannel.ChannelArea := lChannelArea.AreaID;
              RePopulateDataViewer;
            end
            else
              ChannelAreaCbx.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGeneralFlowChannelValidator.OnSummaryOutputClick(Sender: TObject);
const OPNAME = 'TGeneralFlowChannelValidator.OnSummaryOutputClick';
begin
  try
    if(GeneralFlowChannelDialog.SummaryOutputChkBox.HasValueChanged) then
      UpdateChannelIncludeSummary;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

