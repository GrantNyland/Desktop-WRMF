//
//
//  UNIT      : Contains the class TOutputDataSelectionValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/07/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UOutputDataSelectionValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  CommCtrl,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UOutputDataSelectionDialog;

type
  TOutputDataSelectionValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FOnSelectionChange  : TNotifyEvent;
    FOnMoreLessClick    : TNotifyEvent;
    FPopulating         : Boolean;
    FCumulativePlot     : boolean;
    FCondencedPlot      : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SetOnSelectionChange (AOnSelectionChange : TNotifyEvent);
    procedure SetOnMoreLessClick (AOnMoreLessClick : TNotifyEvent);
    procedure DoSelectionChange(Sender: TObject);
    procedure DoSensitivityChange(Sender: TObject);

    procedure DoTimeStepTypeChange(Sender: TObject);
    procedure OnDateTimePickerCloseUp(Sender: TObject);
    procedure OnDateTimePickerChange(Sender: TObject);
    procedure AdjustAverageDates;
    procedure PopulateAveragePeriod;
    procedure PopulateCalendarData;
    procedure RePopulateDataViewer;
  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    procedure PopulateDataViewer; override;
    procedure SetDisplayOptions(AIdentifier: Integer;ANetworkElementType: TNetworkElementType;AOutputSourceDialog: TOutputSourceDialog;
              AOutputDataType: TOutputDataType;AOutputValueType: TOutputValueType);
    function DataSelectionDialog: TOutputDataSelectionDialog;
    property OnSelectionChange : TNotifyEvent read FOnSelectionChange write SetOnSelectionChange;
    property OnMoreLessClick   : TNotifyEvent read FOnMoreLessClick   write SetOnMoreLessClick;
  end;

implementation

uses
  Math,
  SysUtils,
  DateUtils,
  UOutputData,
  UYieldModelDataObject,
  UPlanningModelDataObject,
  UErrorHandlingOperations;

{ TOutputDataSelectionValidator }

procedure TOutputDataSelectionValidator.CreateMemberObjects;
const OPNAME = 'TOutputDataSelectionValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPopulating        := False;
    FOnSelectionChange := nil;
    FOnMoreLessClick   := nil;
    FPanel             := TOutputDataSelectionDialog.Create(nil,FAppModules);

    DataSelectionDialog.LoadCaseSelector.OnSelectionChange := DoSelectionChange;
    DataSelectionDialog.SequenceSelector.OnSelectionChange := DoSelectionChange;
    DataSelectionDialog.MonthSelector.OnSelectionChange    := DoSelectionChange;
    DataSelectionDialog.SelectMonthCombobox.OnChange       := DoSelectionChange;

    DataSelectionDialog.MetersPerSecond.OnClick  := DoSelectionChange;
    DataSelectionDialog.Million.OnClick          := DoSelectionChange;
    DataSelectionDialog.Percentage.OnClick       := DoSelectionChange;

    DataSelectionDialog.LivePercentage.OnClick   := DoSelectionChange;

    DataSelectionDialog.MegaLitersPerDay.OnClick := DoSelectionChange;
    DataSelectionDialog.Meters.OnClick           := DoSelectionChange;
    DataSelectionDialog.MCM.OnClick              := DoSelectionChange;

    DataSelectionDialog.Monthly.OnClick            := DoTimeStepTypeChange;
    DataSelectionDialog.Annually.OnClick           := DoTimeStepTypeChange;
    DataSelectionDialog.MonthlyCummulative.OnClick := DoTimeStepTypeChange;
    DataSelectionDialog.AnnualCummulative.OnClick  := DoTimeStepTypeChange;
    DataSelectionDialog.Sequence.OnClick           := DoTimeStepTypeChange;

    DataSelectionDialog.Supply.OnClick           := DoSelectionChange;
    DataSelectionDialog.Demand.OnClick           := DoSelectionChange;
    DataSelectionDialog.Deficit.OnClick          := DoSelectionChange;
    DataSelectionDialog.DemandAndSupply.OnClick  := DoSelectionChange;
    DataSelectionDialog.Allocated.OnClick        := DoSelectionChange;

    DataSelectionDialog.ChkHighlight.OnClick     := DoSelectionChange;

    DataSelectionDialog.PeriodSequence.OnClick    := DoSelectionChange;
    DataSelectionDialog.PeriodSelection.OnClick   := DoSelectionChange;
    DataSelectionDialog.DatePeriodStart.OnChange  := OnDateTimePickerChange;
    DataSelectionDialog.DatePeriodEnd.OnChange    := OnDateTimePickerChange;
    DataSelectionDialog.DatePeriodStart.OnCloseUp := OnDateTimePickerCloseUp;
    DataSelectionDialog.DatePeriodEnd.OnCloseUp   := OnDateTimePickerCloseUp;
    DataSelectionDialog.ApplySensitivity.OnClick  := DoSensitivityChange;
    DataSelectionDialog.AbsoluteSensitivity.OnClick  := DoSensitivityChange;
    DataSelectionDialog.PercSensitivity.OnClick           := DoSensitivityChange;
    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      DataSelectionDialog.Condenced.OnClick          := DoTimeStepTypeChange;
      DataSelectionDialog.NonCondenced.OnClick       := DoTimeStepTypeChange;

      DataSelectionDialog.AnnualCummulative.OnClick  := DoTimeStepTypeChange;
      DataSelectionDialog.NonCumulative.OnClick      := DoTimeStepTypeChange;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TOutputDataSelectionValidator.DoSensitivityChange(Sender: TObject);
const OPNAME = 'TOutputDataSelectionValidator.DoSensitivityChange';
begin
  try
    if DataSelectionDialog.ApplySensitivity.Checked then
    begin
      DataSelectionDialog.edtSensitivity.Enabled := False;
      DataSelectionDialog.edtPercSensitivity.Enabled := False;
    end
    else
    if DataSelectionDialog.AbsoluteSensitivity.Checked then
    begin
      DataSelectionDialog.edtSensitivity.Enabled := True;
      DataSelectionDialog.edtPercSensitivity.Enabled := False;
    end
    else
    if DataSelectionDialog.PercSensitivity.Checked then
    begin
      DataSelectionDialog.edtSensitivity.Enabled := False;
      DataSelectionDialog.edtPercSensitivity.Enabled := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.DestroyMemberObjects;
const OPNAME = 'TOutputDataSelectionValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelectionValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDataSelectionValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputDataSelection');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.PopulateDataViewer;
const OPNAME = 'TOutputDataSelectionValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.SetDisplayOptions(AIdentifier: Integer;ANetworkElementType: TNetworkElementType;
          AOutputSourceDialog: TOutputSourceDialog;AOutputDataType: TOutputDataType;AOutputValueType: TOutputValueType);
const OPNAME = 'TOutputDataSelectionValidator.SetDisplayOptions';
var
  LMonthSelector           : WordBool;
  LSequenceType            : WideString;
  LViewMonth               : WordBool;
  LViewTypeSupply          : WordBool;
  LViewTypeDemand          : WordBool;
  LViewTypeDeficits        : WordBool;
  LViewTypeDemandAndSupply : WordBool;
  LViewTypeAllocated       : WordBool;
  LMCMPerSecond            : WordBool;
  LMCMPerMonthYear         : WordBool;
  LPercentage              : WordBool;
  LLivePercentage          : WordBool;
  LMegaLitersPerDay        : WordBool;
  LMeters                  : WordBool;
  LMCM                     : WordBool;
  LHighlightDeficit        : WordBool;
  LMonthly                 : WordBool;
  LAnnually                : WordBool;
  LMonthlyCummulative      : WordBool;
  LAnnualCummulative       : WordBool;
  LSequence                : WordBool;
  LCondencedPlot           : WordBool;
  LCumulativePlot          : WordBool;
  LAveragePeriod           : WordBool;
  LChannel                 : IGeneralFlowChannel;
  LReservoirData           : IReservoirData;
  LChannelWithDemand       : boolean;
  LSensitivity             : WordBool;
begin
  try
    FIdentifier := AIdentifier;
    FNetworkElementType := ANetworkElementType;

    DataSelectionDialog.LoadCaseSelector.Enabled      := True;
    DataSelectionDialog.SequenceSelector.Enabled      := True;
    DataSelectionDialog.MonthSelector.Enabled         := True;
    DataSelectionDialog.SelectMonthCombobox.Enabled   := True;
    DataSelectionDialog.SelectMonthlabel.Enabled      := True;
    DataSelectionDialog.DecisionMonthCombobox.Enabled := True;
    DataSelectionDialog.MetersPerSecond.Enabled       := True;
    DataSelectionDialog.Million.Enabled               := True;
    DataSelectionDialog.Percentage.Enabled            := True;
    DataSelectionDialog.LivePercentage.Enabled        := True;
    DataSelectionDialog.Meters.Enabled                := True;
    DataSelectionDialog.MCM.Enabled                   := True;
    DataSelectionDialog.MegaLitersPerDay.Enabled      := True;
    DataSelectionDialog.Monthly.Enabled               := True;
    DataSelectionDialog.Annually.Enabled              := True;
    DataSelectionDialog.MonthlyCummulative.Enabled    := True;
    DataSelectionDialog.AnnualCummulative.Enabled     := True;
    DataSelectionDialog.Sequence.Enabled              := True;
    DataSelectionDialog.Supply.Enabled                := True;
    DataSelectionDialog.Demand.Enabled                := True;
    DataSelectionDialog.Deficit.Enabled               := True;
    DataSelectionDialog.DemandAndSupply.Enabled       := True;
    DataSelectionDialog.Allocated.Enabled             := True;
    DataSelectionDialog.ChkHighlight.Enabled          := True;
    DataSelectionDialog.BtnMoreLess.Enabled           := True;
    DataSelectionDialog.PeriodSelection.Enabled       := True;
    DataSelectionDialog.PeriodSequence.Enabled        := True;
    DataSelectionDialog.DatePeriodStart.Enabled       := True;
    DataSelectionDialog.DatePeriodEnd.Enabled         := True;
    DataSelectionDialog.LblSensitivityBox.Enabled     := True;
    DataSelectionDialog.SensitivityGroupBox.Enabled   := True;
    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      DataSelectionDialog.Condenced.Enabled           := True;
      DataSelectionDialog.NonCondenced.Enabled        := True;
      DataSelectionDialog.MonthlyCummulative.Enabled  := True;
      DataSelectionDialog.AnnualCummulative.Enabled   := True;
      DataSelectionDialog.NonCumulative.Enabled       := True;
      DataSelectionDialog.edtYearsToSkip.Enabled      := True;
    end;

    DataSelectionDialog.LoadCaseSelector.Visible      := True;
    DataSelectionDialog.SequenceSelector.Visible      := True;
    DataSelectionDialog.MonthSelector.Visible         := True;
    DataSelectionDialog.SelectMonthCombobox.Visible   := True;
    DataSelectionDialog.SelectMonthlabel.Visible      := True;
    DataSelectionDialog.DecisionMonthCombobox.Visible := True;
    DataSelectionDialog.MetersPerSecond.Visible       := True;
    DataSelectionDialog.Million.Visible               := True;

    DataSelectionDialog.Percentage.Visible            := True;
    DataSelectionDialog.LivePercentage.Visible        := True;
    DataSelectionDialog.Meters.Visible                := True;
    DataSelectionDialog.MCM.Visible                   := True;
    DataSelectionDialog.MegaLitersPerDay.Visible      := True;
    DataSelectionDialog.Monthly.Visible               := True;
    DataSelectionDialog.Annually.Visible              := True;
    DataSelectionDialog.MonthlyCummulative.Visible    := True;
    DataSelectionDialog.AnnualCummulative.Visible     := True;
    DataSelectionDialog.Sequence.Visible              := True;
    DataSelectionDialog.Supply.Visible                := True;
    DataSelectionDialog.Demand.Visible                := True;
    DataSelectionDialog.Deficit.Visible               := True;
    DataSelectionDialog.DemandAndSupply.Visible       := True;
    DataSelectionDialog.ChkHighlight.Visible          := True;
    DataSelectionDialog.BtnMoreLess.Visible           := True;
    DataSelectionDialog.PeriodSequence.Visible        := True;
    DataSelectionDialog.PeriodSelection.Visible       := True;
    DataSelectionDialog.DatePeriodStart.Visible       := True;
    DataSelectionDialog.LblSensitivityBox.Visible     := True;
    DataSelectionDialog.SensitivityGroupBox.Visible   := True;

    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      DataSelectionDialog.DatePeriodEnd.Visible       := True;
      DataSelectionDialog.Condenced.Visible           := True;
      DataSelectionDialog.NonCondenced.Visible        := True;
      DataSelectionDialog.MonthlyCummulative.Enabled  := True;
      DataSelectionDialog.AnnualCummulative.Visible   := True;
      DataSelectionDialog.NonCumulative.Visible       := True;
      DataSelectionDialog.edtYearsToSkip.Visible      := True;
    end;

    LMonthSelector           := True;
    LViewMonth               := True;
    LViewTypeSupply          := True;
    LViewTypeDemand          := True;
    LViewTypeDeficits        := True;
    LViewTypeDemandAndSupply := True;
    LViewTypeAllocated       := False;
    LMCMPerSecond            := True;
    LMCMPerMonthYear         := True;
    LPercentage              := True;
    LLivePercentage          := True;

    LMegaLitersPerDay        := True;
    LMeters                  := True;
    LMCM                     := True;
    LHighlightDeficit        := True;
    LMonthly                 := True;
    LAnnually                := True;
    LMonthlyCummulative      := True;
    LAnnualCummulative       := True;
    LSequence                := True;
    LAveragePeriod           := True;
    LCondencedPlot           := True;
    LCumulativePlot          := True;
    LSequenceType            := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.RunSequenceType;
    LChannel                 := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[AIdentifier];
    LReservoirData           := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[AIdentifier];
    LChannelWithDemand       := False;
    LSensitivity             := True;

    if(LChannel <> nil) then
      LChannelWithDemand := LChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel];

    case AOutputSourceDialog of
      osdNetworkVisualiser:
      begin
        LViewMonth               := False;
        LViewTypeSupply          := False;
        LViewTypeDemand          := False;
        LViewTypeDeficits        := False;
        LViewTypeDemandAndSupply := False;
        LViewTypeAllocated       := False;
        LMeters                  := False;
        LMCM                     := False;
        LHighlightDeficit        := False;
        LAnnually                := False;
        LMonthlyCummulative      := False;
        LAnnualCummulative       := False;
        LCondencedPlot           := False;
        LCumulativePlot          := False;
        LSensitivity             := False;
      end;
      osdGrid:
      begin
        if (NetworkElementType = votReservoir) or (NetworkElementType = votWetland) then
        begin

          LViewTypeSupply          := False;
          LViewTypeDemand          := False;
          LViewTypeDeficits        := False;
          LViewTypeDemandAndSupply := False;
          LViewTypeAllocated       := False;
          LHighlightDeficit        := False;
          LSensitivity             := False;

          if (AOutputDataType = btMonthEndReservoirVolume) then
          begin
            LMegaLitersPerDay := False;
            LMCMPerSecond     := False;
            LMCMPerMonthYear  := False;
            LMeters           := False;
          end
          else if (AOutputDataType = btMonthEndReservoirElevation) then
          begin
            LMegaLitersPerDay := False;
            LMCMPerSecond     := False;
            LMCMPerMonthYear  := False;
            LMCM              := False;
          end
          else if (AOutputDataType in [btNetBasinRunoffIntoResArea,btRainfallOnReservoirSurface,btGrossEvaporationLossFromReservoir] ) then
          begin
            LMeters           := False;
            LMCM              := False;
            LPercentage := False;
            LLivePercentage := False;
          end;
        end
        else if (NetworkElementType in [votMasterControl,votChannel]) then
        begin
          if (AOutputDataType = btMonthlyAverageChannelFlow) then
          begin
            LViewMonth               := False;
            //LPercentage              := False;
            LLivePercentage          := False;
            LMCM                     := False;
            LMeters                  := False;
            LViewTypeDemand          := False;
            LViewTypeDeficits        := False;
            LViewTypeDemandAndSupply := False;
            LViewTypeAllocated       := False;
            LAnnually                := False;
            LMonthlyCummulative      := False;
            LAnnualCummulative       := False;
            LSequence                := False;
            LHighlightDeficit        := False;
            LAveragePeriod           := False;
            LCondencedPlot           := False;
            LCumulativePlot          := False;
            LSensitivity             := False;
            if (LChannelWithDemand) then
            begin
              LPercentage              := (LChannel.ChannelType <> ctIrrigationBlockInflowChannel);
              LLivePercentage          := False;
              //LViewTypeDemand          := True;
              //LViewTypeDeficits        := True;
              //LViewTypeDemandAndSupply := True;
              //LViewTypeAllocated       := True;
              //LHighlightDeficit        := True;
            end;
            DataSelectionDialog.Monthly.Checked := True;
            DataSelectionDialog.Supply.Checked := True;
          end
        end;
        {case AOutputDataType of
          btMonthlyAverageChannelFlow:
          begin
          end;
          btMonthEndReservoirVolume,
          btMonthEndReservoirElevation,
          btNetBasinRunoffIntoResArea,
          btRainfallOnReservoirSurface,
          btGrossEvaporationLossFromReservoir,
          btMonthlyAverageIrrigationDeficits,
          btMonthlyAverageStackedEnergy:
          begin
          end;
        end;}
      end;
      osdGraph:
      begin
        if (NetworkElementType = votReservoir) or (NetworkElementType = votWetland) then
        begin
          LViewTypeSupply          := False;
          LViewTypeDemand          := False;
          LViewTypeDeficits        := False;
          LViewTypeDemandAndSupply := False;
          LViewTypeAllocated       := False;
          LHighlightDeficit        := False;
          LSensitivity             := False;
          if (AOutputDataType = btMonthEndReservoirVolume) then
          begin
            LMegaLitersPerDay := False;
            LMCMPerSecond     := False;
            LMCMPerMonthYear  := False;
            LMeters           := False;
          end
          else if (AOutputDataType = btMonthEndReservoirElevation) then
          begin
            LMegaLitersPerDay := False;
            LMCMPerSecond     := False;
            LMCMPerMonthYear  := False;
            LMCM              := False;
          end
          else if (AOutputDataType in [btNetBasinRunoffIntoResArea,btRainfallOnReservoirSurface,btGrossEvaporationLossFromReservoir] ) then
          begin
            LMeters           := False;
            LMCM              := False;
            LPercentage := False;
            LLivePercentage := False;
          end;
        end
        else if (NetworkElementType in [votMasterControl,votChannel]) then
        begin
          if (AOutputDataType = btMonthlyAverageChannelFlow) then
          begin
            //LPercentage              := False;
            LLivePercentage          := False;
            LMeters                  := False;
            LMCM                     := False;
            LSequence                := False;
            LHighlightDeficit        := False;
            LAveragePeriod           := False;
            LCondencedPlot           := False;
            LCumulativePlot          := False;
            LSensitivity             := False;
            if (LChannelWithDemand) then
            begin
              LViewTypeDemand          := True;
              LViewTypeDeficits        := True;
              LViewTypeDemandAndSupply := True;
              LViewTypeAllocated       := False;
            end;
          end
        end;
      end;
      osdWaterBalance:
      begin
        LViewMonth               :=  False;
        LViewTypeSupply          := not (ANetworkElementType in [votReservoir,votWetland]);
        LViewTypeDemand          := not (ANetworkElementType in [votReservoir,votWetland]);
        LViewTypeDeficits        := not (ANetworkElementType in [votReservoir,votWetland]);
        LViewTypeDemandAndSupply := not (ANetworkElementType in [votReservoir,votWetland]);
        LViewTypeAllocated       := False;
        LMeters                  :=  False;
        LMCM                     :=  False;
        LCondencedPlot           :=  False;
        LCumulativePlot          :=  False;
        LLivePercentage          := False;
        LSensitivity             := False;
      end;
      osdComplianceGrid:
      begin
        LViewMonth               := False;
        LMeters                  := False;
        LMCM                     := False;
        LViewTypeDemand          := False;
        LViewTypeSupply          := False;
        LViewTypeDemandAndSupply := False;
        LViewTypeAllocated       := False;
        LSequence                := False;
        LHighlightDeficit        := False;
        LAveragePeriod           := False;
        LCondencedPlot           := False;
        LCumulativePlot          := False;
        LLivePercentage          := False;
        DataSelectionDialog.Deficit.Checked := True;
        LSensitivity             := True;
      end;
      osdComplianceGraph:
      begin
        LMonthSelector           := False;
        LLivePercentage          := False;
        LViewTypeSupply          := False;
        LViewTypeDemand          := False;
        LViewTypeDeficits        := False;
        LViewTypeDemandAndSupply := False;
        LViewTypeAllocated       := False;
        LSensitivity             := False;
        if (NetworkElementType in [votMasterControl,votChannel]) then
        begin
          LLivePercentage     := False;
          LPercentage         := False;
          LSensitivity             := False;
          //LPercentage         := (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btRequirementAndSupplyDifference,
          //                            btDefinedAndSimulatedData,btIFRFlow,btMonthlyAverageChannelFlow,btDefinedIFR,
          //                            btIFRRequirementAndFlow,btDefinedreferenceflowVsIFRRelationship,btIFRHistogram]);
          LMegaLitersPerDay   := (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btRequirementAndSupplyDifference,
                                      btDefinedAndSimulatedData,btIFRFlow,btMonthlyAverageChannelFlow,btDefinedIFR,
                                      btIFRRequirementAndFlow,btDefinedreferenceflowVsIFRRelationship,btIFRHistogram]);
          LAnnually                   := not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btRequirementAndSupplyDifference,
                                      btDefinedAndSimulatedData,btIFRFlow,btMonthlyAverageChannelFlow,btDefinedIFR,
                                      btIFRRequirementAndFlow,btDefinedreferenceflowVsIFRRelationship,btIFRHistogram]);
          LViewMonth           := (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btRequirementAndSupplyDifference,
                                      btDefinedAndSimulatedData,btIFRFlow,btMonthlyAverageChannelFlow,btDefinedIFR,
                                      btIFRRequirementAndFlow,btDefinedreferenceflowVsIFRRelationship]);
        end;
        LMeters                := False;
        LMCM                   := False;
        LHighlightDeficit      := False;
        LCondencedPlot         := False;
        LCumulativePlot        := False;
      end;
      osdDistributionCurve:
      begin
        LLivePercentage     := False;
        LSensitivity             := False;
        if (NetworkElementType in [votMasterControl,votChannel]) then
        begin
          if (AOutputDataType = btMonthlyAverageChannelFlow) then
          begin
            //LPercentage              := False;
            LLivePercentage          := False;
            LMeters                  := False;
            LMCM                     := False;
            //LSequence                := False;
            LHighlightDeficit        := False;
            LAveragePeriod           := False;
            LCondencedPlot           := False;
            LCumulativePlot          := False;
          end
        end;
        {LMonthSelector           := False;
        LViewTypeSupply          := (ANetworkElementType <> votReservoir);
        LViewTypeDemand          := (ANetworkElementType <> votReservoir);
        LViewTypeDeficits        := (ANetworkElementType <> votReservoir);
        LViewTypeDemandAndSupply := (ANetworkElementType <> votReservoir);
        LViewTypeAllocated       := False;
        LPercentage              := not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btIFRFlow,
                                                        btMonthlyAverageChannelFlow,btDefinedreferenceflowVsIFRRelationship]);
        LMegaLitersPerDay        := not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btIFRFlow,
                                                        btMonthlyAverageChannelFlow,btDefinedreferenceflowVsIFRRelationship]);
        LMeters                  := False;
        LMCM                     := False;
        LHighlightDeficit        := False;
        LAnnually                := not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btIFRFlow,
                                                        btMonthlyAverageChannelFlow,btDefinedreferenceflowVsIFRRelationship]);
        LCondencedPlot           := False;
        LCumulativePlot          := False;}
      end;
      osdChannelDemands:
      begin
        LMonthSelector           := False;
        LViewMonth               := False;
        LViewTypeSupply          := False;
        LViewTypeDeficits        := False;
        LViewTypeDemandAndSupply := False;
        LViewTypeAllocated       := False;
        LPercentage              := False;
        LLivePercentage          := False;
        LMCM                     := False;
        LMeters                  := False;
        LHighlightDeficit        := False;
        LAnnually                := False;
        LMonthlyCummulative      := False;
        LAnnualCummulative       := False;
        //LSequence                := False;
        LAveragePeriod           := False;
        LCondencedPlot           := False;
        LCumulativePlot          := False;
        LSensitivity             := False;
        DataSelectionDialog.Monthly.Checked := True;
        DataSelectionDialog.Demand.Checked := True;
        if (AOutputDataType = btAnualFirmSelectedYieldDemands) then
        begin
          LViewMonth                   := False;

          LPercentage                  := (LChannel.ChannelType = ctIrrigationBlockInflowChannel) or
                                          (LChannel.ChannelType = ctIFRChannel) or
                                          (LChannel.ChannelType = ctDemandChannel) or
                                          (LChannel.ChannelType = ctMasterControlChannel);

          LMCM                         := False;
          LMeters                      := False;
          LViewTypeDemand              := False;
          LViewTypeDeficits            := False;
          LViewTypeDemandAndSupply     := False;
          LViewTypeAllocated           := False;
          LAnnually                    := False;
          LMonthlyCummulative          := False;
          LAnnualCummulative           := False;
          LMonthSelector               := False;
          LHighlightDeficit            := False;
          LAveragePeriod               := False;
          LCondencedPlot               := False;
          LCumulativePlot              := False;
          LViewTypeSupply              := True;
          LSensitivity                 := False;
          DataSelectionDialog.Monthly.Enabled            := False;
          DataSelectionDialog.Annually.Enabled           := False;
          DataSelectionDialog.MonthlyCummulative.Enabled := False;
          DataSelectionDialog.AnnualCummulative.Enabled  := False;
          DataSelectionDialog.Supply.Checked             := True;
          DataSelectionDialog.Sequence.Checked           := True;
        end;
      end;
      osdWaterUseComplianceGraph: //not created
      begin
        LMonthSelector           := False;
        LViewMonth               := False;
        LViewTypeSupply          := False;
        LViewTypeDemand          := False;
        LViewTypeDeficits        := False;
        LViewTypeDemandAndSupply := False;
        LViewTypeAllocated       := False;
        LMCMPerSecond            := False;
        LMCMPerMonthYear         := False;
        LPercentage              := False;
        LLivePercentage          := False;
        LMegaLitersPerDay        := False;
        LMeters                  := False;
        LMCM                     := False;
        LHighlightDeficit        := False;
        LAnnually                := False;
        LMonthlyCummulative      := False;
        LAnnualCummulative       := False;
        LCondencedPlot           := False;
        LCumulativePlot          := False;
        LSensitivity             := False;
      end;
      osdWRPMGrid:
      begin
        LMonthSelector           := ANetworkElementType in [votReviewTotalSystemStorage,votReviewSubSystemCurtailment];
        LViewMonth               := False;
        LViewTypeSupply          := LChannelWithDemand;
        LViewTypeDemand          := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LViewTypeDeficits        := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LViewTypeDemandAndSupply := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LViewTypeAllocated       := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LMCMPerSecond            := False;
        LMCMPerMonthYear         := False;
        LPercentage              := False;
        LLivePercentage          := False;
        LMegaLitersPerDay        := False;
        LMeters                  := False;
        LMCM                     := False;
        LHighlightDeficit        := False;
        LMonthly                 := not(ANetworkElementType in [votReviewDemandSupply,votReviewTotalSystemStorage]);
        LAnnually                := (ANetworkElementType in [votMasterControl,votChannel,votReviewDemands,votReviewMonthlyChannelResult,votReviewTotalSystemStorage]);
        LMonthlyCummulative      := (ANetworkElementType in [votChannel]);
        LAnnualCummulative       := (ANetworkElementType in [votChannel]);
        LSequence                := not(ANetworkElementType in [votMasterControl,votChannel,votReviewDemandSupply,votReviewSubSystemStorage,votReviewTotalSystemStorage]);
        LCondencedPlot           := (ANetworkElementType = votReviewDemands);
        LCumulativePlot          := (ANetworkElementType in [votChannel,votReviewMonthlyChannelResult]);
        LSensitivity             := False;
      end;
      osdWRPMBoxPlotGrid:
      begin
        LMonthSelector           := ANetworkElementType in [votReviewTotalSystemStorage,votReviewSubSystemCurtailment];
        LViewMonth               := False;
        LViewTypeSupply          := LChannelWithDemand;
        LViewTypeDemand          := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LViewTypeDeficits        := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LViewTypeDemandAndSupply := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LViewTypeAllocated       := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LMCMPerSecond            := (ANetworkElementType = votReviewMonthlyChannelResult);
        LMCMPerMonthYear         := False;
        LPercentage              := False;
        LLivePercentage          := False;
        LMegaLitersPerDay        := False;
        LMeters                  := False;
        LMCM                     := (ANetworkElementType in [votReviewMonthlyChannelResult,votReviewDemandSupply]);
        LHighlightDeficit        := False;
        LMonthly                 := not(ANetworkElementType in [votReviewDemandSupply,votReviewTotalSystemStorage]);
        LAnnually                := (ANetworkElementType in [votMasterControl,votChannel,votReviewDemands,votReviewMonthlyChannelResult,votReviewDemandSupply,votReviewTotalSystemStorage]);
        LMonthlyCummulative      := (ANetworkElementType in [votChannel]);
        LAnnualCummulative       := (ANetworkElementType in [votChannel]);
        LSequence                := not(ANetworkElementType in [votMasterControl,votChannel,votReviewDemandSupply,votReviewSubSystemStorage,votReviewTotalSystemStorage]);
        LCondencedPlot           := (ANetworkElementType = votReviewDemands);
        LCumulativePlot          := (ANetworkElementType in [votChannel,votReviewMonthlyChannelResult]);
        LSensitivity             := False;
      end;
      osdWRPMGraph:
      begin
        LMonthSelector           := ANetworkElementType in [votReviewTotalSystemStorage,votReviewSubSystemCurtailment];
        LViewMonth               := ANetworkElementType in [votReviewMonthlyChannelResult,votReviewDamStorage,
                                                            votReviewSubSystems,votReviewDemands];
        LViewTypeSupply          := LChannelWithDemand;
        LViewTypeDemand          := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LViewTypeDeficits        := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LViewTypeDemandAndSupply := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LViewTypeAllocated       := LChannelWithDemand and (not ANetworkElementType in [votMasterControl,votChannel]);
        LMCMPerSecond            := False;
        LMCMPerMonthYear         := False;
        LPercentage              := False;
        LLivePercentage          := False;
        LMegaLitersPerDay        := False;
        LMeters                  := False;
        LMCM                     := (ANetworkElementType in [votReviewDemandSupply]);;
        LHighlightDeficit        := False;
        LMonthly                 := not(ANetworkElementType in [votReviewDemandSupply,votReviewTotalSystemStorage]);
        LAnnually                := (ANetworkElementType in [votMasterControl,votChannel,votReviewDemands,votReviewMonthlyChannelResult,votReviewDemandSupply,votReviewTotalSystemStorage]);
        LMonthlyCummulative      := (ANetworkElementType in [votChannel]);
        LAnnualCummulative       := (ANetworkElementType in [votChannel]);
        LSequence                := not(ANetworkElementType in [votMasterControl,votChannel,votReviewDemandSupply,votReviewSubSystemStorage,votReviewTotalSystemStorage]);
        LCondencedPlot           := (ANetworkElementType = votReviewDemands);
        LCumulativePlot          := (ANetworkElementType in [votChannel,votReviewMonthlyChannelResult]);
        LSensitivity             := False;
      end;
      osdChannelComparison:
      begin
        LMonthSelector           := False;
        LViewTypeSupply          := False;
        LViewTypeDemand          := False;
        LViewTypeDeficits        := False;
        LViewTypeDemandAndSupply := False;
        LViewTypeAllocated       := False;
        LMeters                  := False;
        LMCM                     := False;
        LHighlightDeficit        := False;
        LCondencedPlot           := False;
        LCumulativePlot          := False;
        LLivePercentage          := False;
        LSensitivity             := False;
      end;
      osdComparisonReservoir:
      begin
        LMonthSelector           := False;
        LViewTypeSupply          := False;
        LViewTypeDemand          := False;
        LViewTypeDeficits        := False;
        LViewTypeDemandAndSupply := False;
        LViewTypeAllocated       := False;
        LMeters                  := False;
        LMCM                     := False;
        LHighlightDeficit        := False;
        LCondencedPlot           := False;
        LCumulativePlot          := False;
        LLivePercentage          := False;
        LSensitivity             := False;
      end;
      osdMonthlyDeficit:
      begin
        LViewMonth               := False;
        LViewTypeSupply          := False;
        LViewTypeDemand          := False;
        LViewTypeDemandAndSupply := False;
        LViewTypeAllocated       := False;
        LMCMPerSecond            := False;
        LMCMPerMonthYear         := False;
        LPercentage              := False;
        LLivePercentage          := False;
        LMegaLitersPerDay        := False;
        LMeters                  := False;
        LMCM                     := False;
        LHighlightDeficit        := False;
        LAnnually                := False;
        LMonthlyCummulative      := False;
        LAnnualCummulative       := False;
        LSequence                := False;
        LAveragePeriod           := False;
        LCondencedPlot           := False;
        LCumulativePlot          := False;
        DataSelectionDialog.Deficit.Checked := True;
        DataSelectionDialog.Monthly.Checked := True;
      end;
      osdDeficitDuration:
      begin
        LViewMonth               := False;
        LViewTypeSupply          := False;
        LViewTypeDemand          := False;
        LViewTypeDemandAndSupply := False;
        LViewTypeAllocated       := False;
        LMCMPerSecond            := False;
        LMCMPerMonthYear         := False;
        LPercentage              := False;
        LLivePercentage          := False;
        LMegaLitersPerDay        := False;
        LMeters                  := False;
        LMCM                     := False;
        LHighlightDeficit        := False;
        LAnnually                := False;
        LMonthlyCummulative      := False;
        LAnnualCummulative       := False;
        LSequence                := False;
        LAveragePeriod           := False;
        LCondencedPlot           := False;
        LCumulativePlot          := False;
        DataSelectionDialog.Deficit.Checked := True;
      end;
      {osdDeficitDuration:
      begin
        LMonthSelector           :=  False;
        LViewTypeSupply          := (ANetworkElementType <> votReservoir);
        LViewTypeDemand          := (ANetworkElementType <> votReservoir);
        LViewTypeDeficits        := (ANetworkElementType <> votReservoir);
        LViewTypeDemandAndSupply := (ANetworkElementType <> votReservoir);
        LViewTypeAllocated       := (ANetworkElementType <> votReservoir);
        if (ANetworkElementType  = votChannel) then
          LPercentage            :=  not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btIFRFlow,
                                       btMonthlyAverageChannelFlow,btDefinedreferenceflowVsIFRRelationship]);
        if (ANetworkElementType  = votChannel) then
          LMegaLitersPerDay      :=  not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btIFRFlow,
                                       btMonthlyAverageChannelFlow,btDefinedreferenceflowVsIFRRelationship]);
        LMeters                  :=  False;
        LMCM                     :=  False;
        LHighlightDeficit        :=  False;
        if (ANetworkElementType  = votChannel) then
          LAnnually              :=  not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btIFRFlow,
                                     btMonthlyAverageChannelFlow,btDefinedreferenceflowVsIFRRelationship]);
        LCondencedPlot           :=  False;
        LCumulativePlot          :=  False;
      end;
      osdMonthlyDeficit:
      begin
        LMonthSelector           :=  False;
        LViewTypeSupply          := (ANetworkElementType <> votReservoir);
        LViewTypeDemand          := (ANetworkElementType <> votReservoir);
        LViewTypeDeficits        := (ANetworkElementType <> votReservoir);
        LViewTypeDemandAndSupply := (ANetworkElementType <> votReservoir);
        LViewTypeAllocated       := (ANetworkElementType <> votReservoir);
        if (ANetworkElementType = votChannel) then
          LPercentage         :=  not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btIFRFlow,
                                       btMonthlyAverageChannelFlow,btDefinedreferenceflowVsIFRRelationship]);
        if (ANetworkElementType = votChannel) then
          LMegaLitersPerDay   :=  not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btIFRFlow,
                                       btMonthlyAverageChannelFlow,btDefinedreferenceflowVsIFRRelationship]);
        LMeters             :=  False;
        LMCM                :=  False;
        LHighlightDeficit   :=  False;
        if (ANetworkElementType = votChannel) then
          LAnnually           :=  not (AOutputDataType in [btIFRRequirement,btIFRRequirementAndSupply,btIFRFlow,
                                     btMonthlyAverageChannelFlow,btDefinedreferenceflowVsIFRRelationship]);
        LCondencedPlot      :=  False;
        LCumulativePlot     :=  False;
     end;}
    end;

    DataSelectionDialog.SequenceSelector.Enabled      := (LSequenceType <> 'H');
    DataSelectionDialog.MonthSelector.Enabled         := LMonthSelector;
    DataSelectionDialog.SelectMonthCombobox.Enabled   := LViewMonth;
    DataSelectionDialog.LoadCaseSelector.Visible      := (FAppModules.Model.ModelName <> CPlanning);
    DataSelectionDialog.DecisionMonthCombobox.Visible := (FAppModules.Model.ModelName = CPlanning);

    DataSelectionDialog.MetersPerSecond.Visible     := LMCMPerSecond;
    DataSelectionDialog.Million.Visible             := LMCMPerMonthYear;

    DataSelectionDialog.Percentage.Visible          := LPercentage;
    DataSelectionDialog.LivePercentage.Visible      := LLivePercentage;
    if LLivePercentage then
      DataSelectionDialog.Percentage.Caption        := FAppModules.Language.GetString('TField.TotalPercentage');

    DataSelectionDialog.MegaLitersPerDay.Visible    := LMegaLitersPerDay;
    DataSelectionDialog.Meters.Visible              := LMeters;
    DataSelectionDialog.MCM.Visible                 := LMCM;

    DataSelectionDialog.Monthly.Enabled             := LMonthly;
    DataSelectionDialog.Annually.Enabled            := LAnnually;
    DataSelectionDialog.MonthlyCummulative.Enabled  := LMonthlyCummulative;
    DataSelectionDialog.AnnualCummulative.Enabled   := LAnnualCummulative;
    DataSelectionDialog.Sequence.Enabled            := LSequence;

    DataSelectionDialog.DecisionMonthCombobox.Enabled := LMonthSelector;

    DataSelectionDialog.Supply.Enabled              := LViewTypeSupply;
    DataSelectionDialog.Demand.Enabled              := LViewTypeDemand;
    DataSelectionDialog.Deficit.Enabled             := LViewTypeDeficits;
    DataSelectionDialog.DemandAndSupply.Enabled     := LViewTypeDemandAndSupply;
    DataSelectionDialog.Allocated.Enabled           := LViewTypeAllocated;

    DataSelectionDialog.ChkHighlight.Enabled        := LHighlightDeficit;
    DataSelectionDialog.PeriodSelection.Enabled     := LAveragePeriod;
    DataSelectionDialog.PeriodSequence.Enabled      := LAveragePeriod;
    DataSelectionDialog.DatePeriodStart.Enabled     := LAveragePeriod;
    DataSelectionDialog.DatePeriodEnd.Enabled       := LAveragePeriod;
    DataSelectionDialog.SensitivityGroupBox.Enabled := LSensitivity;
    DataSelectionDialog.SensitivityGroupBox.Visible := LSensitivity;
    DataSelectionDialog.LblSensitivityBox.Enabled   := LSensitivity;
    DataSelectionDialog.LblSensitivityBox.Visible   := LSensitivity;


    if(FAppModules.Model.ModelName = CPlanning) then
    begin
      DataSelectionDialog.lblCondenced.Enabled       := LCondencedPlot;
      DataSelectionDialog.Condenced.Enabled          := LCondencedPlot;
      DataSelectionDialog.NonCondenced.Enabled       := LCondencedPlot;

      DataSelectionDialog.lblYearsToSkip.Enabled     := LCondencedPlot;
      DataSelectionDialog.edtYearsToSkip.Enabled     := LCondencedPlot;

      DataSelectionDialog.lblCumulative.Enabled      := LCumulativePlot;
      DataSelectionDialog.MonthlyCummulative.Enabled := LCumulativePlot;
      DataSelectionDialog.AnnualCummulative.Enabled  := LCumulativePlot;
      DataSelectionDialog.NonCumulative.Enabled      := LCumulativePlot;
      FCumulativePlot                                := LCumulativePlot;
      FCondencedPlot                                 := LCondencedPlot;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.RePopulateDataViewer;
const OPNAME = 'TOutputDataSelectionValidator.RePopulateDataViewer';
var
  LIndex          : integer;
  LMonthName      : string;
  lLoadCaseCount  : integer;
  lSequenceCount  : integer;
  lMonthCount     : integer;
  lYieldModelData : IYieldModelData;
  LDate           : TDateTime;
  LDataSelection : IOutputDataSelection;
begin
  try
    FPopulating     := True;
    try
      LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
      lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
      lLoadCaseCount  := lYieldModelData.RunConfigurationData.NrOfActiveLoadCases;
      lSequenceCount  := lYieldModelData.RunConfigurationData.NumberOfSequencesInAnalysis;
      lMonthCount     := lYieldModelData.RunConfigurationData.PeriodsInAnalysis;
      DataSelectionDialog.LoadCaseSelector.LoadCaseCount := lLoadCaseCount;
      DataSelectionDialog.SequenceSelector.SequenceCount := lSequenceCount;
      DataSelectionDialog.MonthSelector.MonthCount       := lMonthCount;

      DataSelectionDialog.LoadCaseSelector.CurrentLoadCase := LDataSelection.LoadCase;
      DataSelectionDialog.SequenceSelector.CurrentSequence := LDataSelection.Sequence;
      DataSelectionDialog.MonthSelector.CurrentMonth       := LDataSelection.Month;

      DataSelectionDialog.DecisionMonthCombobox.Items.Clear;
      DataSelectionDialog.DecisionMonthCombobox.Items.Add('Average');
      for LIndex := 1 to lYieldModelData.RunConfigurationData.NrOfDecisionMonths do
      begin
        DataSelectionDialog.DecisionMonthCombobox.Items.Add(IntToStr(lYieldModelData.RunConfigurationData.DecisionMonthByIndex[LIndex]));
      end;
      if(FAppModules.Model.ModelName = CPlanning) and (DataSelectionDialog.DecisionMonthCombobox.Items.Count > 0) then
      begin
        if LDataSelection.DecisionMonth = 0 then
          DataSelectionDialog.DecisionMonthCombobox.ItemIndex := 0
        else
          DataSelectionDialog.DecisionMonthCombobox.ItemIndex := DataSelectionDialog.DecisionMonthCombobox.Items.IndexOf(IntToStr(LDataSelection.DecisionMonth));
      end;
      PopulateCalendarData;
      DataSelectionDialog.SelectMonthCombobox.Items.Add('ALL');
      for LIndex := 1 to 12 do
      begin
        LMonthName := lYieldModelData.RunConfigurationData.MonthNameByIndex[LIndex];
        DataSelectionDialog.SelectMonthCombobox.Items.Add(LMonthName);
      end;
      if (Ord(LDataSelection.Units) = ouPerSecond) AND (DataSelectionDialog.MetersPerSecond.Visible) then
        DataSelectionDialog.MetersPerSecond.Checked := True
      else if ((Ord(LDataSelection.Units) = ouMcmPerMonthOrYear) AND (DataSelectionDialog.Million.Visible)) then
        DataSelectionDialog.Million.Checked := True
     else if ((Ord(LDataSelection.Units) = ouMegaLitersPerDay) AND (DataSelectionDialog.MegaLitersPerDay.Visible)) then
        DataSelectionDialog.MegaLitersPerDay.Checked := True
      else if (Ord(LDataSelection.Units) = ouPercentage) AND (DataSelectionDialog.Percentage.Visible) then
        DataSelectionDialog.Percentage.Checked := True

      else if (Ord(LDataSelection.Units) = ouLivePercentage) AND (DataSelectionDialog.LivePercentage.Visible) then
        DataSelectionDialog.LivePercentage.Checked := True

      else if (Ord(LDataSelection.Units) = ouMCM) AND (DataSelectionDialog.MCM.Visible) then
        DataSelectionDialog.MCM.Checked := True
      else if (Ord(LDataSelection.Units) = ouMeters) AND (DataSelectionDialog.Meters.Visible) then
        DataSelectionDialog.Meters.Checked := True
      else if (DataSelectionDialog.MetersPerSecond.Visible) then
        DataSelectionDialog.MetersPerSecond.Checked := True
      else if (DataSelectionDialog.Million.Visible) then
        DataSelectionDialog.Million.Checked := True
      else if (DataSelectionDialog.MCM.Visible) then
        DataSelectionDialog.MCM.Checked := True
      else if (DataSelectionDialog.MegaLitersPerDay.Visible) then
        DataSelectionDialog.MegaLitersPerDay.Checked := True
      else if (DataSelectionDialog.Meters.Visible) then
        DataSelectionDialog.Meters.Checked := True
      else if (DataSelectionDialog.Percentage.Visible) then
        DataSelectionDialog.Percentage.Checked := True
      else if (DataSelectionDialog.LivePercentage.Visible) then
        DataSelectionDialog.LivePercentage.Checked := True;

      if (Ord(LDataSelection.ValueType) = ovtSupply) AND (DataSelectionDialog.Supply.Enabled) then
        DataSelectionDialog.Supply.Checked := True
      else if (Ord(LDataSelection.ValueType) = ovtDemand) AND (DataSelectionDialog.Demand.Enabled) then
        DataSelectionDialog.Demand.Checked := True
      else if (Ord(LDataSelection.ValueType) = ovtDeficits) AND (DataSelectionDialog.Deficit.Enabled) then
        DataSelectionDialog.Deficit.Checked := True
      else if (Ord(LDataSelection.ValueType) = ovtDemandAndSupply) AND (DataSelectionDialog.DemandAndSupply.Enabled) then
        DataSelectionDialog.DemandAndSupply.Checked := True
      else if (Ord(LDataSelection.ValueType) = ovtAllocated) AND (DataSelectionDialog.Allocated.Enabled) then
        DataSelectionDialog.Allocated.Checked := True;

      if (Ord(LDataSelection.TimeStep) = otsMonthly) then
        DataSelectionDialog.Monthly.Checked := True
      else if (Ord(LDataSelection.TimeStep) = otsAnnual) AND (DataSelectionDialog.Annually.Enabled) then
        DataSelectionDialog.Annually.Checked := True
      else if (Ord(LDataSelection.TimeStep) = otsMonthlyCumulative) AND (DataSelectionDialog.MonthlyCummulative.Enabled) then
        DataSelectionDialog.MonthlyCummulative.Checked := True
      else if (Ord(LDataSelection.TimeStep) = otsAnnualCumulative) AND (DataSelectionDialog.AnnualCummulative.Enabled) then
        DataSelectionDialog.AnnualCummulative.Checked := True
      else if (Ord(LDataSelection.TimeStep) = otsSequence) AND (DataSelectionDialog.Sequence.Enabled) then
        DataSelectionDialog.Sequence.Checked := True
      else
        DataSelectionDialog.Monthly.Checked := True;

      DataSelectionDialog.ChkHighlight.Checked   := LDataSelection.Highlight;

      DataSelectionDialog.SelectMonthCombobox.ItemIndex := LDataSelection.DisplayMonth;
      LDate := TYieldModelDataObject(FAppModules.Model.ModelData).ModelCalendar.CalenderStartDate[0];
      DataSelectionDialog.DatePeriodStart.MinDate := LDate;
      DataSelectionDialog.DatePeriodEnd.MinDate   := LDate;
      LDate := IncMonth(LDate,TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.YearsInAnalysis*12);
      DataSelectionDialog.DatePeriodStart.MaxDate := LDate;
      DataSelectionDialog.DatePeriodEnd.MaxDate   := LDate;
      PopulateAveragePeriod;
      DataSelectionDialog.ApplySensitivity.Checked := (Ord(LDataSelection.ApplySensitivity) = stvNone);
      DataSelectionDialog.AbsoluteSensitivity.Checked := (Ord(LDataSelection.ApplySensitivity) = stvAbsolute);
      DataSelectionDialog.PercSensitivity.Checked := (Ord(LDataSelection.ApplySensitivity) = stvPercentage);
      DataSelectionDialog.edtSensitivity.Text := FloatToStr(LDataSelection.Sensitivity);
      DataSelectionDialog.edtPercSensitivity.Text := FloatToStr(LDataSelection.PercSensitivity);

      if(FAppModules.Model.ModelName = CPlanning) then
      begin
        DataSelectionDialog.NonCumulative.Checked := True;
        if (DataSelectionDialog.Condenced.Enabled) and (Ord(LDataSelection.PlotOption) = poCondenced)then
        begin
          DataSelectionDialog.Condenced.Checked := True;
          DataSelectionDialog.lblYearsToSkip.Enabled := True;
          DataSelectionDialog.edtYearsToSkip.Enabled := True;
        end
        else
        if (DataSelectionDialog.NonCondenced.Enabled) and (Ord(LDataSelection.PlotOption) = poNotCondenced)then
          DataSelectionDialog.NonCondenced.Checked := True
        else
          DataSelectionDialog.NonCondenced.Checked := True;

        //if (DataSelectionDialog.AnnualCummulative.Enabled) and (Ord(LDataSelection.PlotOption) = poCumulative)then
        //  DataSelectionDialog.AnnualCummulative.Checked := True
        //else
        if (DataSelectionDialog.NonCumulative.Enabled) and (Ord(LDataSelection.PlotOption) = poNotCumulative)then
          DataSelectionDialog.NonCumulative.Checked := True
        else
          DataSelectionDialog.NonCondenced.Checked := True;
      end;
    finally
      FPopulating := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelectionValidator.Initialise: boolean;
const OPNAME = 'TOutputDataSelectionValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    DoSensitivityChange(nil);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDataSelectionValidator.DataSelectionDialog : TOutputDataSelectionDialog;
const OPNAME = 'TOutputDataSelectionValidator.DataSelectionDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputDataSelectionDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.DoTimeStepTypeChange (Sender: TObject);
const OPNAME = 'TOutputDataSelectionValidator.DoTimeStepTypeChange';
var
  LDataSelection : IOutputDataSelection;
begin
  try
    LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
    DoSelectionChange(Sender);
    with DataSelectionDialog do
    begin
      SelectMonthCombobox.Enabled := True;
      if (Monthly.Checked) then
      begin
        Million.Caption := FAppModules.Language.GetString('LabelText.Millionm3');
        if(FAppModules.Model.ModelName = CPlanning) then
        begin
          DataSelectionDialog.lblYearsToSkip.Enabled := False;
          DataSelectionDialog.edtYearsToSkip.Enabled := False;
          if (FCumulativePlot) then
          begin
            DataSelectionDialog.MonthlyCummulative.Enabled := True;
            DataSelectionDialog.AnnualCummulative.Enabled := True;
            DataSelectionDialog.NonCumulative.Enabled := True;
          end;
          if (FCondencedPlot) then
          begin
            DataSelectionDialog.Condenced.Enabled := True;
            DataSelectionDialog.Condenced.Enabled := True;
            DataSelectionDialog.NonCondenced.Enabled := True;
            DataSelectionDialog.NonCondenced.Enabled := True;
            if DataSelectionDialog.Condenced.Checked then
            begin
              DataSelectionDialog.lblYearsToSkip.Enabled := True;
              DataSelectionDialog.edtYearsToSkip.Enabled := True;
              DataSelectionDialog.edtYearsToSkip.Text := IntToStr(LDataSelection.YearsToSkip);
            end;
          end;

        end;
      end
      else
      begin
        if Annually.Checked then
        begin
          SelectMonthCombobox.ItemIndex := 0;
          SelectMonthCombobox.Enabled := False;
        end;

        Million.Caption := FAppModules.Language.GetString('LabelText.MillionInYear');
        if(FAppModules.Model.ModelName = CPlanning) then
        begin
          DataSelectionDialog.lblYearsToSkip.Enabled := False;
          DataSelectionDialog.edtYearsToSkip.Enabled := False;
          if (FCumulativePlot) then
          begin
            DataSelectionDialog.MonthlyCummulative.Enabled := True;
            DataSelectionDialog.AnnualCummulative.Enabled := True;
            DataSelectionDialog.NonCumulative.Enabled := True;
          end;
          if (FCondencedPlot) then
          begin
            DataSelectionDialog.Condenced.Enabled := False;
            DataSelectionDialog.Condenced.Enabled := False;
            DataSelectionDialog.NonCondenced.Enabled := False;
            DataSelectionDialog.NonCondenced.Enabled := False;

            DataSelectionDialog.lblYearsToSkip.Enabled := False;
            DataSelectionDialog.edtYearsToSkip.Enabled := False;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.DoSelectionChange (Sender: TObject);
const OPNAME = 'TOutputDataSelectionValidator.DoSelectionChange';
begin
  try
    if not FPopulating then
    begin
      FPopulating     := True;
      try
        AdjustAverageDates;
        if Assigned(FOnSelectionChange) then
        begin
          FOnSelectionChange(Self);
        end;
        PopulateCalendarData;
      finally
        FPopulating := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputDataSelectionValidator.AdjustAverageDates;
const OPNAME = 'TOutputDataSelectionValidator.AdjustAverageDates';
var
  LDate             : TDateTime;
begin
  try
    LDate             := DataSelectionDialog.DatePeriodStart.DateTime;
    DataSelectionDialog.DatePeriodStart.DateTime := EncodeDate(YearOf(LDate), MonthOf(LDate), 1);
    LDate             := DataSelectionDialog.DatePeriodEnd.DateTime;
    DataSelectionDialog.DatePeriodEnd.DateTime   := EncodeDate(YearOf(LDate), MonthOf(LDate), 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.SetOnSelectionChange(AOnSelectionChange: TNotifyEvent);
const OPNAME = 'TOutputDataSelectionValidator.SetOnSelectionChange';
begin
  try
    FOnSelectionChange := AOnSelectionChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.SetOnMoreLessClick (AOnMoreLessClick : TNotifyEvent);
const OPNAME = 'TOutputDataSelectionValidator.SetOnMoreLessClick';
begin
  try
    DataSelectionDialog.BtnMoreLess.OnClick := AOnMoreLessClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.PopulateAveragePeriod;
const OPNAME = 'TOutputDataSelectionValidator.PopulateAveragePeriod';
var
  LDataSelection : IOutputDataSelection;
begin
  try
    FPopulating  := True;
    try
      LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
      DataSelectionDialog.DatePeriodStart.DateTime  := LDataSelection.AverageStartDate;
      DataSelectionDialog.DatePeriodEnd.DateTime    := LDataSelection.AverageEndDate;
      if(LDataSelection.AverageType = oatPeriod) then
      begin
        DataSelectionDialog.PeriodSelection.Checked := True;
        DataSelectionDialog.SetPeriodControlsState(True);
      end
      else
      begin
        DataSelectionDialog.PeriodSequence.Checked := True;
        DataSelectionDialog.SetPeriodControlsState(False);
      end;
    finally
      FPopulating := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.OnDateTimePickerCloseUp(Sender: TObject);
const OPNAME = 'TOutputDataSelectionValidator.OnDateTimePickerCloseUp';
begin
  try
    DoSelectionChange(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.OnDateTimePickerChange(Sender: TObject);
const OPNAME = 'TOutputDataSelectionValidator.OnDateTimePickerChange';
begin
  try
    if(Sender is TDateTimePicker) then
    begin
      if not TDateTimePicker(Sender).DroppedDown then
        DoSelectionChange(Sender);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDataSelectionValidator.PopulateCalendarData;
const OPNAME = 'TOutputDataSelectionValidator.PopulateCalendarData';
var
  LDate     : TDateTime;
  LStrDate  : string;
  LMonth    : integer;
begin
  try
    LMonth   := DataSelectionDialog.MonthSelector.CurrentMonth-1;
    LDate    := TYieldModelDataObject(FAppModules.Model.ModelData).ModelCalendar.CalenderDateFromPeriodElapsed[LMonth];
    LStrDate := FormatDateTime('mmm - yyyy',LDate);
    DataSelectionDialog.LblCalendarDate.Caption := LStrDate;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
end.

