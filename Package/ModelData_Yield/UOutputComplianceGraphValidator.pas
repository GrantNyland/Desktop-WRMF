//
//
//  UNIT      : Contains the class TOutputComplianceGraphValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputComplianceGraphValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Graphics,
  VCLTee.TeEngine,
  VCLTee.Series,
  VCLTee.Chart,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UIFRFeatures,
  UOutputComplianceGraphDialog;

type
  TOutputComplianceGraphValidator = class(TAbstractOutputDialogValidator)
  protected
    FCurrentViewData : TOutputDataType;
    FPrevViewData    : TOutputDataType;
    FDisplayMonth    : integer;
    FUseUnits        : TOutputUnits;
    FValueType       : TOutputValueType;
    FMonthlyData     : TStringList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnViewDataTypeChange(Sender: TObject);
    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure PopulateDialogSelectors;
    procedure RePopulateDataViewer;
    procedure ClearChart;
    procedure SetCurrentViewData;
    function  ElementName: string;
    procedure ChangeViewDataLabel;
    function GetScenarioWhereClause: string;
    procedure PopulateDamLevelSeries(ALineSeries: TLineSeries;ADemandFileName: string; AMinX,AMaxX: TDateTime);
    procedure PopulateReservoirChannelData(AReservoirData: TStrings);
    procedure PopulateReservoirStorageData(AData: TStrings);
    procedure PopulateReservoirStorageChartData(AData: TStrings);

    procedure PopulateRequirementAndFlow(AIFRFeature : TIFRFeature);
    procedure PopulateReferenceFlow(AIFRFeature : TIFRFeature);
    procedure PopulateIFRRequirementAndSupply(AIFRFeature : TIFRFeature; AData: TStrings);
    procedure PopulateRequirementAndSupplyDifference(AIFRFeature : TIFRFeature; AData: TStrings);
    procedure PopulateDefinedAndSimulatedData(AIFRFeature : TIFRFeature; AData: TStrings);

    procedure PopulateIFRHistogram(AIFRFeature : TIFRFeature;AData : TStrings);
    procedure PopulateDemandSupplyChannels(AChannel : IGeneralFlowChannel;AData: TStrings);
    procedure PopulateDemandSupplyValues(AData: TStrings;AChannel : IGeneralFlowChannel);
    procedure PopulateChannelDemandData(AValues: TStringList; AChannel: IGeneralFlowChannel);
    function CalculateAverageValues(AData: TStrings): boolean;

    procedure PopulateDefinedAndRequired(AIFRFeature : TIFRFeature);
    procedure DrawColorBands;
    procedure GetSelectionData;
    procedure GetSortedDataByMonth(AData: TStrings;AMonth : integer);
    procedure GetSortedAllData(AData: TStrings);
    function GetSupplyValueByYear(ASupplyYear : integer;AMonth : integer;AData: TStrings) : double;
    procedure SetChartLegend(ALegendAlignment: TLegendAlignment; ALegendVisible: boolean);
    function ConvertToMcM(AValue : double; AIndex : integer) : double;
    procedure GetMonthData(AData: TStrings;AMonth : integer);
    function GetSupplyByMonthYear(ASupplyData : TStrings;AMonth,AYear : integer) : double;
    procedure GetChartLegend(AChart: TFieldChart);
    function GetMonthDescription(AMonthIndex: integer): string;
    procedure GetSortedDataForAllMonths(AData: TStrings);
    procedure DisplayLegend(AChart : TFieldChart;ASeriesA,ASeriesB : string);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    function GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    function GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ViewDialog: TOutputComplianceGraphDialog;

  end;

implementation

uses
  SysUtils,
  UConstants,
  UWetland,
  UOutputData,
  UDataSetType,
  UYieldModelDataObject,
  UErrorHandlingOperations,
  UStringListOfStringLists,
  Math,
  VCL.Dialogs,
  UUtilities, URunConfigurationData;

{ TOutputComplianceGraphValidator }

procedure TOutputComplianceGraphValidator.CreateMemberObjects;
const OPNAME = 'TOutputComplianceGraphValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputComplianceGraphDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.cmbViewDataType.OnSelect:= OnViewDataTypeChange;
    ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.OnSelect := OnViewDataTypeChange;

    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    FMonthlyData                        := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.DestroyMemberObjects;
const OPNAME = 'TOutputComplianceGraphValidator.DestroyMemberObjects';
begin
  try
    FreeAndNil(FMonthlyData);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TOutputComplianceGraphValidator.Initialise: boolean;
const OPNAME = 'TOutputComplianceGraphValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    FPrevViewData    := btNone;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputComplianceGraphValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'LOADCASESCOUNT') or (UpperCase(AFieldName) = 'HYDROSEQCOUNT')
      or (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputComplianceGraphValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComplianceGraphValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputComplianceGraph');
    
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.SaveState: boolean;
const OPNAME = 'TOutputComplianceGraphValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.ViewDialog : TOutputComplianceGraphDialog;
const OPNAME = 'TOutputComplianceGraphValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputComplianceGraphDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputComplianceGraphValidator.GetNextSignificantRecord';
begin
  Result := 0;
  try
    Result := ACurrentRecord  + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputComplianceGraphValidator.GetPreviousSignificantRecord';
begin
  Result := 0;
  try
    Result := ACurrentRecord  - 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.ClearDataViewer;
const OPNAME = 'TOutputComplianceGraphValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearChart;
    ViewDialog.cmbViewDataType.Items.Clear;
    if ViewDialog.ReservoirChannelComparitor.Visible then
      ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.PopulateDataViewer;
const OPNAME = 'TOutputComplianceGraphValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputComplianceGraphValidator.PopulateDialogSelectors';
var
  LViewData    : string;
  LIndex       : integer;
  LSelectIndex : integer;
  LChannel : IGeneralFlowChannel;
  LChannelList   : IChannelList;
  LChannelNumber : integer;
begin
  try
    if (FIdentifier >= 0)  and (NetworkElementType <> votNone)then
    begin
      case NetworkElementType of
        votMasterControl:
        begin
          LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphMonthlyAverageSuppliedVsRequired');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRHistogram)));

          {LViewData := 'Water Balance';//FAppModules.Language.GetString('OutputReview.OutputGraphDifferenceSuppliedVsRequired');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btRequirementAndSupplyDifference)));
          }
        end;
        votReservoir:
        begin
          LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphReservoirChannel');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btReservoirChannel)));
//          ViewDialog.DisplayString := 'Volume';

          LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphReservoirChannelVolume');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btReservoirChannel)));

          LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphStorageVolumeNorm');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btReservoirStorage)));

          LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphStorageVolumeNormStorage');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btReservoirStorage)));
          
          {LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphStorageVolumeBoxP');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirElevation)));}

          LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
          if LChannelList <> nil then
          begin
            for LIndex := 0 to LChannelList.ChannelCount-1 do
            begin
              LChannel := LChannelList.ChannelByIndex[LIndex];
              ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.AddItem(LChannel.ChannelName,TObject(LChannel.ChannelNumber));
            end;

            if TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
              MasterControlFeatureList.MasterControlFeatureCount > 0 then
            begin
              LChannelNumber := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MasterControlFeatureList.
              MasterControlFeatureByIndex[0].Channel.ChannelNumber;
              LIndex := ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.Items.IndexOfObject(TObject(LChannelNumber));
              if LIndex >=0 then
                ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.ItemIndex :=  LIndex;
            end;
          end;
        end;
        votNodeWithInflow:
        begin
        end;
        votNodeWithoutInflow:
        begin
        end;
        votChannel:
        begin
          LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData
                 .ChannelList.ChannelByChannelNumber[FIdentifier];
          if (LChannel <> nil) then
          begin

            if((LChannel.ChannelType = 8)) or
              (LChannel.ChannelType = 2) or (LChannel.ChannelType = 11) or
              (LChannel.ChannelType = 14) then
            begin

              {LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphSuppliedVsRequired');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRRequirementAndSupply)));

              LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphDefinedIFRVsSupplied');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btDefinedAndSimulatedData)));

              LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphDefinedIFRVsRequired');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btDefinedAndRequired)));

              LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphRequiredVsReferenceFlow');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRRequirementAndFlow)));

              LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphDifferenceSuppliedVsRequired');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btRequirementAndSupplyDifference)));
              }
              LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphMonthlyAverageSuppliedVsRequired');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRHistogram)));

              {LViewData := 'Water Balance';//FAppModules.Language.GetString('OutputReview.OutputGraphDifferenceSuppliedVsRequired');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btRequirementAndSupplyDifference)));
              }
            end;

          end;
        end;
        votIrrigationArea:
        begin
        end;
        votPowerPlant:
        begin
        end;
        votWetland:
        begin
          LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphReservoirChannel');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btReservoirChannel)));
          LViewData := FAppModules.Language.GetString('OutputReview.OutputGraphStorageVolumeNorm');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btReservoirStorage)));
          LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
          if LChannelList <> nil then
          begin
            for LIndex := 0 to LChannelList.ChannelCount-1 do
            begin
              LChannel := LChannelList.ChannelByIndex[LIndex];
              ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.AddItem(LChannel.ChannelName,TObject(LChannel.ChannelNumber));
            end;
            if TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
              MasterControlFeatureList.MasterControlFeatureCount > 0 then
            begin
              LChannelNumber := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MasterControlFeatureList.
              MasterControlFeatureByIndex[0].Channel.ChannelNumber;
              LIndex := ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.Items.IndexOfObject(TObject(LChannelNumber));
              if LIndex >=0 then
                ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.ItemIndex :=  LIndex;
            end;
          end;
        end;
      end;
      if(ViewDialog.cmbViewDataType.Items.Count > 0) then
      begin
        //ViewDialog.cmbViewDataType.ItemIndex := 0;
        LSelectIndex := -1;
        if (FPrevViewData <> btNone) then
        begin
          for LIndex := 0 to ViewDialog.cmbViewDataType.Items.Count - 1 do
          begin
            if (integer(FPrevViewData) = integer(ViewDialog.cmbViewDataType.Items.Objects[LIndex])) then
            begin
              LSelectIndex := LIndex;
              Break;
            end;
          end;
        end;
        if (LSelectIndex >= 0 ) then
          ViewDialog.cmbViewDataType.ItemIndex := LSelectIndex
        else
        if (ViewDialog.cmbViewDataType.Items.Count > 0) then
          ViewDialog.cmbViewDataType.ItemIndex := 0;
        SetCurrentViewData;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.RePopulateDataViewer;
const OPNAME = 'TOutputComplianceGraphValidator.RePopulateDataViewer';
var
  LDataContainer: TStringList;
  LErrors: string;
  //LIFRFeature : TIFRFeature;
  LChannel : IGeneralFlowChannel;
begin
  try
    ClearChart;
    GetSelectionData;
    if(FIdentifier      >= 0) and
      (NetworkElementType   <> votNone) and
      (FCurrentViewData <> btNone) then
    begin
      LDataContainer := TStringList.Create;
      try
        LErrors := '';
        ViewDialog.ShowError(LErrors);
        case FCurrentViewData of
          btReservoirChannel:
          begin
            if ViewDialog.cmbViewDataType.ItemIndex = 1 then
            begin
              if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
                 LDataContainer,btMonthEndReservoirVolume,FIdentifier,LErrors)) then
                PopulateReservoirChannelData(LDataContainer)
              else
                ViewDialog.ShowError(LErrors);
            end
            else
            begin
              if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
                  LDataContainer,btMonthEndReservoirElevation,FIdentifier,LErrors)) then
                PopulateReservoirChannelData(LDataContainer)
              else
                ViewDialog.ShowError(LErrors);
            end;
          end;
          btReservoirStorage:
          begin
            if ViewDialog.cmbViewDataType.ItemIndex = 2 then
            begin
              if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
                 LDataContainer,btMonthEndReservoirElevation,FIdentifier,LErrors)) then
                PopulateReservoirStorageData(LDataContainer)
              else
                ViewDialog.ShowError(LErrors);
            end
            else
              if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
                 LDataContainer,btMonthEndReservoirVolume,FIdentifier,LErrors)) then
                PopulateReservoirStorageData(LDataContainer)
              else
                ViewDialog.ShowError(LErrors);
          end;
          btMonthEndReservoirElevation :
          begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
               LDataContainer,FCurrentViewData,FIdentifier,LErrors)) then
              PopulateReservoirStorageChartData(LDataContainer)
            else
              ViewDialog.ShowError(LErrors);
          end;

{---------------------------------------------------------------------------------------------------------------------}

          {btIFRRequirementAndSupply:
          begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
              LDataContainer,btMonthlyAverageChannelFlow,FIdentifier,LErrors)) then
            begin

              LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                             CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
              PopulateIFRRequirementAndSupply(LIFRFeature,LDataContainer);
            end
            else
              ViewDialog.ShowError(LErrors);
          end;
          btRequirementAndSupplyDifference:
          begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
              LDataContainer,btMonthlyAverageChannelFlow,FIdentifier,LErrors)) then
            begin

              LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                             CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
              PopulateRequirementAndSupplyDifference(LIFRFeature,LDataContainer);
            end
            else
              ViewDialog.ShowError(LErrors);
          end;

          btDefinedAndRequired :
            begin
              LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                              CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
              PopulateDefinedAndRequired(LIFRFeature);
            end;

          btDefinedAndSimulatedData:
            begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
                LDataContainer,btMonthlyAverageChannelFlow,FIdentifier,LErrors)) then
              begin
                LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                               CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
                PopulateDefinedAndSimulatedData(LIFRFeature,LDataContainer);
              end;
            end;

          btIFRRequirementAndFlow:
            begin
              LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                             CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
              PopulateRequirementAndFlow(LIFRFeature);
            end;
          }
          btIFRHistogram:
            begin
              if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
                LDataContainer,btMonthlyAverageChannelFlow,FIdentifier,LErrors)) then
              begin
                {LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                               CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
                if LIFRFeature <> nil then
                  PopulateIFRHistogram(LIFRFeature,LDataContainer)
                else
                begin}
                  LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
                  if LChannel <> nil then
                    PopulateDemandSupplyChannels(LChannel,LDataContainer);

                //end;
              end;

            end;
{-------------------------------------------------------------------------------------------------------------------}
        end;
      finally
        LDataContainer.Free;
      end;
      ChangeViewDataLabel;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.PopulateDemandSupplyChannels(AChannel : IGeneralFlowChannel;AData: TStrings);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateDemandSupplyChannels';
begin
  try
    if (AChannel <> nil) then
    begin
      if(AChannel.ChannelType = 2) then
        PopulateDemandSupplyValues(AData,AChannel)
      else if(AChannel.ChannelType = 8) {and (AChannel.IFRFeature = nil)} then
        PopulateDemandSupplyValues(AData,AChannel)
      else if(AChannel.ChannelType = 11) then
        PopulateDemandSupplyValues(AData,AChannel)
      else if(AChannel.ChannelType = 14) then
        PopulateDemandSupplyValues(AData,AChannel);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.PopulateDemandSupplyValues(AData: TStrings;AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateDemandSupplyChannels';
var
  LChannelDemandValues : TStringList;
  LIndex : integer;
  LMonthName : string;
  LSupply,
  LDemand : double;
  LDemandData : TStringList;
  LSupplyData : TStringList;
  lMonthDays  : array [1..12] of double;
  lYieldModelData : IYieldModelData;
begin
  try
    LChannelDemandValues := TStringList.Create;
    LDemandData := TStringList.Create;
    LSupplyData := TStringList.Create;
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRChannelComplienceGraph.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PrepareChart;
    if FUseUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('OutputReview.OutputGraphSupplyAndDemand'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else if FUseUnits = ouMegaLitersPerDay then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('OutputReview.OutputGraphSupplyAndDemand'),
                           [FAppModules.Language.GetString('MasterControl.MegaL')])
    else if FUseUnits = ouPerSecond then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('OutputReview.OutputGraphSupplyAndDemand'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Title.Caption  := '';
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Title.Text.Text           := '';
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Increment      := 1;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.RequiredBarSeries.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.ActualBarSeries.Visible   := True;
    try
      PopulateChannelDemandData(LChannelDemandValues,AChannel);
      if CalculateAverageValues(AData) then
      begin
        if (LChannelDemandValues.Count=AData.Count) then
        begin
          LDemandData.Clear;
          LSupplyData.Clear;
          LDemandData.CommaText := LChannelDemandValues[LChannelDemandValues.Count-1];
          LSupplyData.CommaText := AData[AData.Count-1];
          lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
          for LIndex := 1 to 12 do
          begin
            lMonthDays[lIndex] := LYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
          end;
          for LIndex := 1 to 12 do
          begin
            LDemand := StrToFloatDef(LDemandData[LIndex],0);
            LSupply := StrToFloatDef(LSupplyData[LIndex],0);

            if (FUseUnits = ouMcmPerMonthOrYear) then
            begin
              LDemand  := LDemand * (lMonthDays[LIndex] * 86400.0) /1000000.0;
              LSupply  := LSupply * (lMonthDays[LIndex] * 86400.0) /1000000.0;
            end
            else if (FUseUnits = ouMegaLitersPerDay) then
            begin
              LDemand  := LDemand * (86400.0) / 1000.0;
              LSupply  := LSupply * (86400.0) / 1000.0;
            end;

            LMonthName := TYieldModelDataObject(FAppModules.Model.ModelData).
                          CastRunConfigurationData.MonthNameByIndex[LIndex];
            ViewDialog.IFRChannelComplienceGraph.RequiredBarSeries.AddXY(LIndex,LDemand,LMonthName,clBlue);
            ViewDialog.IFRChannelComplienceGraph.ActualBarSeries.AddXY(LIndex,LSupply,LMonthName,clRed);
          end;
          GetChartLegend(ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart);
        end;
      end;
    finally
      FreeAndNil(LChannelDemandValues);
      FreeAndNil(LDemandData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.PopulateChannelDemandData(AValues: TStringList; AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateChannelDemandData';
var
  LErrors                : string;
begin
  try
    AValues.Clear;
    if{(FValueType in [ovtDeficits,ovtDemand,ovtDemandAndSupply])  and}
     (AChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel]) then
    begin
      if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDemandOutputData.GetChannelDemandValues(
         AValues,AChannel.ChannelNumber,LErrors) then
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.CalculateAverageValues(AData: TStrings): boolean;
const OPNAME = 'TOutputComplianceGraphValidator.CalculateAverageValues';
var
  LRow,
  LCol           : integer;
  LYearCount     : integer;
  LDataContainer : TStringList;
  LYear          : integer;
  LValues        : array[1..13] of double;
  LDaysPerMonth  : array[1..13] of double;
  LMonthlyTotals : array[1..13] of double;
  LMonthValue    : double;
  LTotal         : double;
  LGrandTotal    : double;
  lYieldModelData : IYieldModelData;
begin
  Result := False;
  try
    LYearCount := AData.Count;
    if(LYearCount = 0) then
    begin
      Result := True;
      Exit;
    end;

    LDataContainer := TStringList.Create;
    try
      lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
      for LCol := 1 to 12 do
        LDaysPerMonth[LCol] := lYieldModelData.RunConfigurationData.MonthDaysByIndex[LCol];
      LDaysPerMonth[13] := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;

      for LCol := 1 to 12 do
        LMonthlyTotals[LCol] := 0.0;

      LGrandTotal := 0.0;
      for LRow := 0 to LYearCount-1 do
      begin
        LDataContainer.CommaText := AData[LRow];
        if(LDataContainer.Count >= 13) then
        begin
          LYear  := StrToIntDef(LDataContainer[0],0);
          LTotal      := 0.0;
          for LCol := 1 to 12 do
          begin
            LMonthValue := StrToFloatDef(LDataContainer[LCol],0.0);
            LTotal := LTotal + LMonthValue*LDaysPerMonth[LCol];
            LValues[LCol] := LMonthValue;
            LMonthlyTotals[LCol] := LMonthlyTotals[LCol]+ LMonthValue;
          end;
          LTotal      := LTotal/LDaysPerMonth[13];
          LValues[13] := LTotal;;
          LGrandTotal := LGrandTotal + LTotal;
          AData[LRow] := IntToStr(LYear) +','+ DoubleArrayToCommaText(LValues,3);
        end;
      end;
      LMonthlyTotals[13] := LGrandTotal;
      for LCol := 1 to 13 do
        LMonthlyTotals[LCol] := LMonthlyTotals[LCol]/ LYearCount;
      AData.Add('Average,'+DoubleArrayToCommaText(LMonthlyTotals,3));
    finally
      LDataContainer.Free;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TOutputComplianceGraphValidator.ChangeViewDataLabel;
const OPNAME = 'TOutputComplianceGraphValidator.ChangeViewDataLabel';
begin
  try
    case ViewDialog.cmbViewDataType.ItemIndex of
      0: begin
           ViewDialog.DisplayString := 'Level';
           ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('TField.m')+')';
           ViewDialog.ReservoirChannelComparitor.ReservoirChart.LeftAxis.Title.Caption   := FAppModules.Language.GetString('OutputReview.OutputGraphStorageVolumeNorm')+
                                                                                            '('+FAppModules.Language.GetString('TField.m')+')';
           ViewDialog.ReservoirChannelComparitor.ReservoirChart.BottomAxis.Title.Caption := FAppModules.Language.GetString('OutputComplianceGraph.ReservoirTimeTitle');
           ViewDialog.ReservoirChannelComparitor.ChannelChart.LeftAxis.Title.Caption     := FAppModules.Language.GetString('GridHeading.Flow')+
                                                                                            '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
           ViewDialog.ReservoirChannelComparitor.ChannelChart.BottomAxis.Title.Caption   := FAppModules.Language.GetString('OutputComplianceGraph.ChannelTimeTitle');
         end;
      1: begin
           ViewDialog.DisplayString := 'Volume';
           ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('TField.MCM')+')';
           ViewDialog.ReservoirChannelComparitor.ReservoirChart.LeftAxis.Title.Caption   := FAppModules.Language.GetString('OutputReview.OutputGraphStorageVolumeNormStorage')+
                                                                                            ' ('+FAppModules.Language.GetString('TField.MCM')+')';
           ViewDialog.ReservoirChannelComparitor.ReservoirChart.BottomAxis.Title.Caption := FAppModules.Language.GetString('OutputComplianceGraph.ReservoirTimeTitle');
           ViewDialog.ReservoirChannelComparitor.ChannelChart.LeftAxis.Title.Caption     := FAppModules.Language.GetString('GridHeading.Flow')+
                                                                                            ' ('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
           ViewDialog.ReservoirChannelComparitor.ChannelChart.BottomAxis.Title.Caption   := FAppModules.Language.GetString('OutputComplianceGraph.ChannelTimeTitle');
         end;
      2: begin
           ViewDialog.DisplayString := 'Level';
           ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('TField.m')+')';
           ViewDialog.ReservoirStorageComparitor.ReservoirChart.LeftAxis.Title.Caption   := FAppModules.Language.GetString('OutputReview.OutputGraphStorageVolumeNorm')+
                                                                                            ' ('+FAppModules.Language.GetString('TField.m')+')';
           ViewDialog.ReservoirStorageComparitor.ReservoirChart.BottomAxis.Title.Caption := FAppModules.Language.GetString('OutputComplianceGraph.ReservoirTimeTitle');
         end;
      3: begin
           ViewDialog.DisplayString := 'Volume';
           ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('TField.MCM')+')';
           ViewDialog.ReservoirStorageComparitor.ReservoirChart.LeftAxis.Title.Caption   := FAppModules.Language.GetString('OutputReview.OutputGraphStorageVolumeNormStorage')+
                                                                                            ' ('+FAppModules.Language.GetString('TField.MCM')+')';
           ViewDialog.ReservoirStorageComparitor.ReservoirChart.BottomAxis.Title.Caption := FAppModules.Language.GetString('OutputComplianceGraph.ReservoirTimeTitle');
         end;
    end;
    ViewDialog.ReservoirChannelComparitor.ReservoirChart.LeftAxis.Title.Repaint;
    ViewDialog.ReservoirChannelComparitor.ReservoirChart.BottomAxis.Title.Repaint;
    ViewDialog.ReservoirChannelComparitor.ChannelChart.LeftAxis.Title.Repaint;
    ViewDialog.ReservoirChannelComparitor.ChannelChart.BottomAxis.Title.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.SetCurrentViewData;
const OPNAME = 'TOutputComplianceGraphValidator.SetCurrentViewData';
var
  LIndex: integer;
begin
  try
    if (ViewDialog.cmbViewDataType.ItemIndex >= 0) then
    begin
      LIndex := Integer(ViewDialog.cmbViewDataType.Items.Objects[ViewDialog.cmbViewDataType.ItemIndex]);
      FCurrentViewData := TOutputDataType(LIndex);
    end
    else
      FCurrentViewData := btNone;
    FPrevViewData := FCurrentViewData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputComplianceGraphValidator.OnViewDataTypeChange';
begin
  try
    SetCurrentViewData;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.ClearChart;
const OPNAME = 'TOutputComplianceGraphValidator.ClearChart';
begin
  try
    ViewDialog.ReservoirChannelComparitor.ClearChart;
    ViewDialog.ReservoirStorageComparitor.ClearChart;
    ViewDialog.ReservoirStorageComparitor.ConfigureSeries;
    ViewDialog.ReservoirStorageGraph.ClearChart;
    ViewDialog.IFRChannelComplienceGraph.ClearChart;
    ViewDialog.ReservoirChannelComparitor.Visible := False;
    ViewDialog.ReservoirStorageComparitor.Visible := False;
    ViewDialog.ReservoirStorageGraph.Visible      := False;
    ViewDialog.IFRChannelComplienceGraph.Visible  := False;
    ViewDialog.IFRChannelComplienceGraph.Initialise;

    ViewDialog.ReservoirChannelComparitor.ReservoirChart.LeftAxis.Title.Caption   := '';
    ViewDialog.ReservoirChannelComparitor.ReservoirChart.BottomAxis.Title.Caption := '';
    ViewDialog.ReservoirChannelComparitor.ReservoirChart.Title.Text.Clear;

    ViewDialog.ReservoirChannelComparitor.ChannelChart.LeftAxis.Title.Caption    := '';
    ViewDialog.ReservoirChannelComparitor.ChannelChart.BottomAxis.Title.Caption  := '';
    ViewDialog.ReservoirChannelComparitor.ChannelChart.Title.Text.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.ElementName: string;
const OPNAME = 'TOutputComplianceGraphValidator.ElementName';
begin
  Result := '';
  try
    case FNetworkElementType of
      votMasterControl:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier].ChannelName;
      end;
      votReservoir:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier].ReservoirConfigurationData.ReservoirName;
      end;
      votNodeWithInflow:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.NodeWithInflowByIdentifier[FIdentifier].ReservoirConfigurationData.ReservoirName;
      end;
      votNodeWithoutInflow:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.NodeWithoutInflowByIdentifier[FIdentifier].ReservoirConfigurationData.ReservoirName;
      end;
      votChannel:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier].ChannelName;
      end;
      votIrrigationArea:
      begin
      end;
      votPowerPlant:
      begin
      end;
      votWetland:
      begin
        Result := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.WetlandList.WetlandByNodeNumber[FIdentifier].Name;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.PopulateReservoirChannelData(AReservoirData : TStrings);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateReservoirChannelData';
var
  LMonthNumbers  : array[0..11] of integer;
  LYear,
  LStartMonth,
  LIndex,
  //LChannelIndex,
  LCount: integer;
  LMonthlyValues: TStringList;
  LMonthlyValue : double;
  LDate: TDateTime;
  LReservoirData : IReservoirData;
  LDrawDownLevels,
  LChannelNumber : integer;
  LFullStorageLvl,
  LDeadStorageLvl,
  LBottomOfRes,
  LMinXValue,
  LMaxXValue,
  LYValue : double;
  LChannelData : TStringList;
  LChannel : IGeneralFlowChannel;
  LErrors : string;
  //LIntercepts : TInterceptValues;
  LMonthDays        : TMonthDaysArray;
  LTotalValues      : TStringList;
  LFlowCapability   : double;
  LWetland : TWetland;
begin
  try
    ClearChart;
    if(AReservoirData.Count > 0) then
    begin
      ViewDialog.ReservoirChannelComparitor.Visible := True;
      LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      LWetland := nil;
      if (NetworkElementType = votWetland) then
        LWetland := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastWetlandList.CastWetlandByNodeNumber(FIdentifier);

      if Assigned(LReservoirData) or Assigned(LWetland) then
      begin
        if (NetworkElementType <> votWetland) and (LWetland = nil) then
        begin
          if ViewDialog.cmbViewDataType.ItemIndex = 1 then
          begin
            LDrawDownLevels := LReservoirData.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount;
            LFullStorageLvl := LReservoirData.GetReservoirVolumeByElevation(LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation);
            LDeadStorageLvl := LReservoirData.GetReservoirVolumeByElevation(LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation);
            LBottomOfRes    := LReservoirData.GetReservoirVolumeByElevation(LReservoirData.ReservoirZoneElevationsData.BottomOfReservoir.Elevation);
            ViewDialog.ReservoirChannelComparitor.OperatingLevels := LDrawDownLevels;
          end
          else
          begin
            LDrawDownLevels := LReservoirData.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount;
            LFullStorageLvl := LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
            LDeadStorageLvl := LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
            LBottomOfRes    := LReservoirData.ReservoirZoneElevationsData.BottomOfReservoir.Elevation;
            ViewDialog.ReservoirChannelComparitor.OperatingLevels := LDrawDownLevels;
          end;
        end
        else
        begin
          LDrawDownLevels := 0;
          LFullStorageLvl := 0;
          LDeadStorageLvl := 0;
          LBottomOfRes    := 0;
          ViewDialog.ReservoirChannelComparitor.OperatingLevels := LDrawDownLevels;
        end;

        SetLength(LMonthDays, Length(TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthsDaysArray));
        LMonthDays      := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthsDaysArray;
        //LChannelCount   := 0;

        LChannelNumber := integer(ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.Items.Objects[ViewDialog.ReservoirChannelComparitor.ChannelsComboBox.ItemIndex]);

        ViewDialog.ReservoirChannelComparitor.ChannelCount := 1;
        ViewDialog.ReservoirChannelComparitor.PrepareChart;
        LStartMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartMonthNumber-1;
        for LIndex := 0 to 11 do
        begin
          LStartMonth := LStartMonth + 1;
          if(LStartMonth > 12) then
             LStartMonth := 1;
          LMonthNumbers[LIndex] := LStartMonth;
        end;

        LMonthlyValues := TStringList.Create;
        LChannelData   := TStringList.Create;
        LTotalValues   := TStringList.Create;
        try
          for LIndex := 0 to AReservoirData.Count -1 do
          begin
            LMonthlyValues.CommaText := AReservoirData.Strings[LIndex];
            LYear := StrToInt(LMonthlyValues.Strings[0]);
            for LCount := 1 to LMonthlyValues.Count -2 do
            begin
              LDate := EncodeDate(LYear,LMonthNumbers[LCount-1],1);
              LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);
              ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationTimeSeries.AddXY(
                LDate,LMonthlyValue);
              if(LMonthNumbers[LCount-1] = 12) then
                LYear := LYear + 1;
            end;
          end;
          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationTimeSeries.Active := True;

          LMinXValue := ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationTimeSeries.XValues.MinValue;
          LMaxXValue := ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationTimeSeries.XValues.MaxValue;

          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationFSLSeries.AddXY(LMinXValue, LFullStorageLvl);
          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationFSLSeries.AddXY(LMaxXValue, LFullStorageLvl);
          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationFSLSeries.Active := True;

          for LIndex := 0 to LDrawDownLevels -1 do
          begin
            LYValue := LReservoirData.ReservoirZoneElevationsData.DrawDownLevelByIndex[LIndex].AverageElevations;
            ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationOPLSeries[LIndex].Clear;
            ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationOPLSeries[LIndex].AddXY(LMinXValue, LYValue);
            ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationOPLSeries[LIndex].AddXY(LMaxXValue, LYValue);
            ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationOPLSeries[LIndex].Active := True;
          end;

          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationDSLSeries.AddXY(LMinXValue, LDeadStorageLvl);
          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationDSLSeries.AddXY(LMaxXValue, LDeadStorageLvl);
          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationDSLSeries.Active := True;

          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationBORSeries.AddXY(LMinXValue, LBottomOfRes);
          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationBORSeries.AddXY(LMaxXValue, LBottomOfRes);
          ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationBORSeries.Active := True;

          ViewDialog.ReservoirChannelComparitor.ReservoirFullStorageVolumeSeries.AddXY(LMinXValue, LFullStorageLvl);
          ViewDialog.ReservoirChannelComparitor.ReservoirFullStorageVolumeSeries.AddXY(LMaxXValue, LFullStorageLvl);
          ViewDialog.ReservoirChannelComparitor.ReservoirFullStorageVolumeSeries.Active := True;

          {for LIndex := 0 to LDrawDownLevels -1 do
          begin
            LYValue := LReservoirData.ReservoirZoneElevationsData.DrawDownLevelByIndex[LIndex].AverageElevations;
            if not CompareDouble(LFullStorageLvl, LYValue, 2) then
            begin
              ViewDialog.ReservoirChannelComparitor.ReservoirOperatingLevelSeries[LIndex].Clear;
              ViewDialog.ReservoirChannelComparitor.ReservoirOperatingLevelSeries[LIndex].AddXY(LMinXValue, LYValue);
              ViewDialog.ReservoirChannelComparitor.ReservoirOperatingLevelSeries[LIndex].AddXY(LMaxXValue, LYValue);
              ViewDialog.ReservoirChannelComparitor.ReservoirOperatingLevelSeries[LIndex].Active := True;
            end;
          end;
           }
          ViewDialog.ReservoirChannelComparitor.ReservoirDeadStorageVolumeSeries.AddXY(LMinXValue, LDeadStorageLvl);
          ViewDialog.ReservoirChannelComparitor.ReservoirDeadStorageVolumeSeries.AddXY(LMaxXValue, LDeadStorageLvl);
          ViewDialog.ReservoirChannelComparitor.ReservoirDeadStorageVolumeSeries.Active := True;
          ViewDialog.ReservoirChannelComparitor.RefreshChartAxisDisplay(ViewDialog.ReservoirChannelComparitor.ReservoirChart);

          //==================================================================
          //DrawColorBands;

          {for LChannelIndex := 0 to LChannelCount - 1 do
          begin
            LChannelNumber := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByIndex[LChannelIndex].Channel.ChannelNumber;
            LChannel       := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.MinMaxFlowConstraintList.MinMaxFlowConstraintByIndex[LChannelIndex].Channel;
            if (LChannel.UpStreamNodeNumber = FIdentifier) then
            begin
          }
            if LChannelNumber > 0 then
            begin
              LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkElementData.ChannelList.ChannelByChannelNumber[LChannelNumber];
              if LChannel <> nil then
              begin
                if (TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(LChannelData,btMonthlyAverageChannelFlow,LChannelNumber,LErrors)) then
                begin
                  for LIndex := 0 to LChannelData.Count -1 do
                  begin
                    LMonthlyValues.CommaText := LChannelData[LIndex];
                    LYear := StrToInt(LMonthlyValues[0]);
                    for LCount := 1 to LMonthlyValues.Count -2 do
                    begin
                      LDate := EncodeDate(LYear,LMonthNumbers[LCount-1],1);
                      LMonthlyValue := StrToFloat(LMonthlyValues[LCount]);
                      ViewDialog.ReservoirChannelComparitor.ChannelFlowTimeSeries[0].AddXY(LDate,LMonthlyValue);
                      if(LMonthNumbers[LCount-1] = 12) then
                        LYear := LYear + 1;
                    end;
                  end;
                  if LChannelData.Count>0 then
                  begin
                    LMonthlyValues.CommaText := LChannelData[LChannelData.Count - 1];
                    LFlowCapability          := StrToFloat(LMonthlyValues[LMonthlyValues.Count - 1]);

                    LMinXValue := ViewDialog.ReservoirChannelComparitor.ChannelFlowTimeSeries[0].XValues.MinValue;
                    LMaxXValue := ViewDialog.ReservoirChannelComparitor.ChannelFlowTimeSeries[0].XValues.MaxValue;

                    ViewDialog.ReservoirChannelComparitor.ChannelFlowCapabilitySeries[0].AddXY(LMinXValue, LFlowCapability);
                    ViewDialog.ReservoirChannelComparitor.ChannelFlowCapabilitySeries[0].AddXY(LMaxXValue, LFlowCapability);
                    ViewDialog.ReservoirChannelComparitor.ChannelFlowTimeSeries[0].Active := True;
                  end;
                end;
              end;

            end;
          //end;
          ViewDialog.ReservoirChannelComparitor.RefreshChartAxisDisplay(ViewDialog.ReservoirChannelComparitor.ChannelChart);
        finally
          LMonthlyValues.Free;
          LChannelData.Free;
          LTotalValues.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.PopulateReservoirStorageData(AData: TStrings);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateReservoirStorageData';
var
  LMonthNumbers   : array[0..11] of integer;
  LYear           : integer;
  LStartMonth     : integer;
  LIndex          : integer;
  LCount          : integer;
  LMonthlyValues  : TStringList;
  LMonthlyValue   : double;
  LReservoirData  : IReservoirData;
  LFullStorageLvl : double;
  LDeadStorageLvl : double;
  LMinXValue      : double;
  LMaxXValue      : double;
  lMonthLabel     : string;
  lZoneCount      : integer;
  lZoneElevations : array[1..12] of double;
  lMonth          : integer;
  lZoneSeries     : TLineSeries;
begin
  try
    ClearChart;
    if (AData.Count > 0) then
    begin
      ViewDialog.ReservoirStorageComparitor.Visible := True;
      ViewDialog.ReservoirStorageComparitor.Visible := True;
      ViewDialog.ReservoirStorageComparitor.ReservoirChart.Visible := True;
      ViewDialog.ReservoirStorageComparitor.ReservoirChart.Enabled := True;

      LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      if Assigned(LReservoirData) then
      begin
        if ViewDialog.cmbViewDataType.ItemIndex = 2 then
        begin
          LFullStorageLvl := LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
          LDeadStorageLvl := LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
        end
        else
        begin
          LFullStorageLvl := LReservoirData.GetReservoirVolumeByElevation(LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation);
          LDeadStorageLvl := LReservoirData.GetReservoirVolumeByElevation(LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation);
        end;

        LStartMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartMonthNumber-1;
        for LIndex := 0 to 11 do
        begin
          LStartMonth := LStartMonth + 1;
          if (LStartMonth > 12) then
            LStartMonth := 1;
          LMonthNumbers[LIndex] := LStartMonth;
        end;

        LMonthlyValues := TStringList.Create;
        try
          for LIndex := 0 to AData.Count -1 do
          begin
            LMonthlyValues.CommaText := AData[LIndex];
            LYear := StrToInt(LMonthlyValues[0]);
            for LCount := 1 to LMonthlyValues.Count -2 do
            begin
              if ((FDisplayMonth = 0) OR (FDisplayMonth = LCount)) then
              begin
                LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);
                lMonthLabel   := IntToStr(LYear) + '/' + Format('%2.2d', [LMonthNumbers[lCount-1]]);
                ViewDialog.ReservoirStorageComparitor.ReservoirStorageVolumeSimSeries.AddY(LMonthlyValue, lMonthLabel);

                if (LMonthNumbers[LCount-1] = 12) then
                LYear := LYear + 1;
              end;
            end;
          end;

          lZoneCount := LReservoirData.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount;
          ViewDialog.ReservoirStorageComparitor.CreateZoneSeries(lZoneCount);
          for lIndex := 0 to lZoneCount - 1 do {for every zone}
          begin
            lZoneSeries := ViewDialog.ReservoirStorageComparitor.ZoneElevationSeriesByIndex[LIndex];
            for lMonth := 1 to 12 do
            begin
              lZoneElevations[lMonth] := LReservoirData.ReservoirZoneElevationsData.
                                         DrawDownLevelByIndex[lIndex].MonthlyElevationByIndex[lMonth];
            end;

            for LCount := 1 to AData.Count do {for every year}
            begin
              for lMonth := 1 to 12 do
              begin
                if ((FDisplayMonth = 0 ) OR (FDisplayMonth = lMonth)) then
                  lZoneSeries.AddY(lZoneElevations[lMonth], '');
              end;
            end;
          end;

          ViewDialog.ReservoirStorageComparitor.ReservoirStorageVolumeActSeries.Clear;
          if(LReservoirData.ReservoirConfigurationData.DamLevelsFileName <> '') then
            PopulateDamLevelSeries(ViewDialog.ReservoirStorageComparitor.ReservoirStorageVolumeActSeries,
                                   LReservoirData.ReservoirConfigurationData.DamLevelsFileName,
                                   ViewDialog.ReservoirStorageComparitor.ReservoirStorageVolumeSimSeries.MinXValue,
                                   ViewDialog.ReservoirStorageComparitor.ReservoirStorageVolumeSimSeries.MaxXValue);

          ViewDialog.ReservoirStorageComparitor.ReservoirStorageVolumeSimSeries.Active := True;
          ViewDialog.ReservoirStorageComparitor.ReservoirStorageVolumeActSeries.Active := True;

          LMinXValue := ViewDialog.ReservoirStorageComparitor.ReservoirStorageVolumeSimSeries.XValues.MinValue;
          LMaxXValue := ViewDialog.ReservoirStorageComparitor.ReservoirStorageVolumeSimSeries.XValues.MaxValue;

          ViewDialog.ReservoirStorageComparitor.ReservoirFullStorageVolumeSeries.AddXY(LMinXValue, LFullStorageLvl);
          ViewDialog.ReservoirStorageComparitor.ReservoirFullStorageVolumeSeries.AddXY(LMaxXValue, LFullStorageLvl);
          ViewDialog.ReservoirStorageComparitor.ReservoirFullStorageVolumeSeries.Active := True;

          ViewDialog.ReservoirStorageComparitor.ReservoirDeadStorageVolumeSeries.AddXY(LMinXValue, LDeadStorageLvl);
          ViewDialog.ReservoirStorageComparitor.ReservoirDeadStorageVolumeSeries.AddXY(LMaxXValue, LDeadStorageLvl);
          ViewDialog.ReservoirStorageComparitor.ReservoirDeadStorageVolumeSeries.Active := True;
          ViewDialog.ReservoirStorageComparitor.RefreshChartAxisDisplay(LFullStorageLvl + 10);
        finally
          LMonthlyValues.Free;
        end;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.SetChartLegend(ALegendAlignment: TLegendAlignment; ALegendVisible: boolean);
const OPNAME = 'TOutputComplianceGraphValidator.SetChartLegend';
var
  LIndex : integer;
begin
  try
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Alignment := ALegendAlignment;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Visible   := ALegendVisible;
    for LIndex := 2 to ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.SeriesCount - 1 do
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Series.Visible := False;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TOutputComplianceGraphValidator.PopulateReferenceFlow(AIFRFeature: TIFRFeature);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateReferenceFlow';
var
  LStringValue: string;
  LFloatValue : double;
  LReferenceFlowData,
  LMonthData,
  LLineData   : TStringList;
  LYValue,
  LPercentile: double;
  LIndex : integer;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRChannelComplienceGraph.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PrepareChart;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Title.Caption :=
                   FAppModules.Language.GetString('TIFRSiteDialog.ExceedenceProbability');
    if FUseUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedReferenceFlow'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUseUnits = ouPerSecond then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedReferenceFlow'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
    if(AIFRFeature = nil) then
      Exit;
    if (FDisplayMonth = 0) then
      Exit;

    LMonthData         := TStringList.Create;
    LLineData          := TStringList.Create;
    LReferenceFlowData := TStringList.Create;
    try
      if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
        Exit;

      LMonthData.Sorted := True;
      LMonthData.Duplicates := dupAccept;
      for LIndex := 0 to LReferenceFlowData.Count-1 do
      begin
        LLineData.CommaText := LReferenceFlowData.Strings[LIndex];
        LStringValue    := LLineData[FDisplayMonth];
        LFloatValue     := StrToFloat(LStringValue);
        LMonthData.Add(FormatFloat('00000000000000.000',LFloatValue));
      end;

      LLineData.Clear;
      for LIndex := LMonthData.Count-1 downto 0 do
        LLineData.Add(LMonthData[LIndex]);

      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.SetMinMax(0,100);
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Increment := 10;
      ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Pointer.Style := psCircle;
      ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Visible := True;
      ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Visible := True;

      for LIndex := 0 to LLineData.Count-1 do
      begin
        LPercentile := (LIndex/LLineData.Count)*100;
        LYValue := StrToFloat(LLineData[LIndex]);
        LYValue := ConvertToMcM(LYValue,FDisplayMonth);
        ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.AddXY(LPercentile,LYValue,'',clRed);
        ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.AddXY(LPercentile,LYValue,'',clRed);
      end;
    finally
      LMonthData.Free;
      LLineData.Free;
      LReferenceFlowData.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.PopulateRequirementAndFlow(AIFRFeature : TIFRFeature);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateRequirementAndFlow';
var
  LIndex,
  LMonth,
  LCount   : integer;
  LPercentile : double;
  LReference,
  LRequirement : double;
  LRequiredData,
  LMonthData,
  LReferenceFlowData : TStringList;
  LYValue  : double;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRChannelComplienceGraph.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PrepareChart;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Title.Caption :=
                           FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
      if FUseUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRAndRefFlow'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUseUnits = ouPerSecond then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRAndRefFlow'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);

    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.SetMinMax(0,100);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Increment := 10;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Visible := True;
    //ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Increment := 1 ;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
    if (AIFRFeature <> nil) then
    begin
      LReferenceFlowData := TStringList.Create;
      LMonthData         := TStringList.Create;
      LRequiredData      := TStringList.Create;
      try
        if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
          Exit;

        if LReferenceFlowData.Count > 0 then
        begin
          if (FDisplayMonth > 0) then
          begin
            GetSortedDataByMonth(LReferenceFlowData,FDisplayMonth);
            for LCount := 0 to LReferenceFlowData.Count-1 do
            begin
              LRequirement := AIFRFeature.GetRequirementFlowFromReferenceFlow(FDisplayMonth,StrToFloat(LReferenceFlowData[LCount]));
              LPercentile := (LCount/LReferenceFlowData.Count)*100;
              if (LRequirement <> NullFloat) then
              begin
                LRequirement := ConvertToMcM(LRequirement,FDisplayMonth);
                ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].Color := clMaroon;
                ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].Color := clMaroon;
                ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].Pointer.Style := psCircle;
                ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].AddXY(LPercentile,LRequirement,'',clMaroon);
                ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].Title :=
                FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR');
                ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].AddXY(LPercentile,LRequirement,'',clMaroon);
                ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].Pen.Width := 2;

              end;
              if (StrToFloat(LReferenceFlowData[LCount]) <> NullFloat) then
              begin
                LReference := StrToFloat(LReferenceFlowData[LCount]);
                LReference := ConvertToMcM(LReference,FDisplayMonth);
                ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayTwo[FDisplayMonth-1].Color := clBlue;
                ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayTwo[FDisplayMonth-1].Color := clBlue;
                ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayTwo[FDisplayMonth-1].Pointer.Style := psCircle;
                ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayTwo[FDisplayMonth-1].AddXY(LPercentile,LReference,'',clBlue);
                ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayTwo[FDisplayMonth-1].Title :=
                FAppModules.Language.GetString('OutputReview.OutputGraphReferenceFlow');
                ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayTwo[FDisplayMonth-1].AddXY(LPercentile,LReference,'',clBlue);
                ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayTwo[FDisplayMonth-1].Pen.Width := 2;
              end;
            end;
          end
          else
          if (FDisplayMonth = 0) then
          begin
            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Clear;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Clear;

            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Visible := True;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Visible := True;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Pen.Width := 2;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Pen.Width := 2;
            for LMonth := 1 to 12 do
            begin
              LReferenceFlowData.Clear;
              if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
                Exit;
              GetMonthData(LReferenceFlowData,LMonth);
              for LIndex := 0 to LReferenceFlowData.Count-1 do
              begin
                LRequirement := AIFRFeature.GetRequirementFlowFromReferenceFlow(LMonth,StrToFloat(LReferenceFlowData[LIndex]));
                LRequirement := ConvertToMcM(LRequirement,LMonth);
                LYValue := StrToFloat(FormatFloat('00000000000000.000',StrToFloat(LReferenceFlowData[LIndex])));
                LYValue := ConvertToMcM(LYValue,LMonth);
                LRequiredData.Add(FloatToStr(LRequirement));
                LMonthData.Add(FloatToStr(LYValue));
              end;
            end;
            GetSortedDataForAllMonths(LMonthData);
            GetSortedDataForAllMonths(LRequiredData);
            for LIndex := 0 to LMonthData.Count-1 do
            begin
              LPercentile := (LIndex/LMonthData.Count)*100;
              LYValue     := StrToFloat(LMonthData[LIndex]);
              LRequirement := StrToFloat(LRequiredData[LIndex]);
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.AddXY(LPercentile,LRequirement,'',clMaroon);
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Title :=
              FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR');
              ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.AddXY(LPercentile,LYValue,'',clBlue);
              ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Title :=
              FAppModules.Language.GetString('OutputReview.OutputGraphReferenceFlow');
              ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Color := clBlue;
            end;
          end;
        end;
        DisplayLegend(ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart,FAppModules.Language.GetString('OutputReview.OutputGraphReferenceFlow')
                       ,FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR'));
      finally
        LReferenceFlowData.Free;
        LMonthData.Free;
        LRequiredData.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.PopulateRequirementAndSupplyDifference(AIFRFeature : TIFRFeature; AData: TStrings);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateRequirementAndSupplyDifference';
var
  LMonth,
  LCount   : integer;
  LSupplyValue,
  LPercentile,
  LFloatValue,
  LDifference : double;
  LRequirement : double;
  LDifferenceData,
  LSortedDifferenceData,
  LUnRankedReferenceFlowData : TStringList;
  LReferenceFlowData : TStringList;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRChannelComplienceGraph.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PrepareChart;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Title.Caption :=
                           FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
    if FUseUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.DifferenceIFRAndSupplyFlow'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUseUnits = ouPerSecond then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.DifferenceIFRAndSupplyFlow'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);

    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.SetMinMax(0,100);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Increment := 10;
    ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Color := clRed;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Pointer.Style := psCircle;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Color := clRed;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
    if (AIFRFeature <> nil) then
    begin
      LReferenceFlowData := TStringList.Create;
      LDifferenceData    := TStringList.Create;
      LDifferenceData.Sorted := True;
      LDifferenceData.Duplicates := dupAccept;
      LSortedDifferenceData   := TStringList.Create;
      LUnRankedReferenceFlowData := TStringList.Create;
      try
        LReferenceFlowData.Clear;
        if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
          Exit;
        if LReferenceFlowData.Count > 0 then
        begin
          if (FDisplayMonth > 0) then
          begin
            GetSortedDataByMonth(LReferenceFlowData,FDisplayMonth);
            if AData.Count <> LReferenceFlowData.Count then
              Exit;
            for LCount := 0 to LReferenceFlowData.Count-1 do
            begin
              LRequirement := StrToFloat(FormatFloat('00000000000000.000',
                AIFRFeature.GetRequirementFlowFromReferenceFlow(FDisplayMonth,StrToFloat(LReferenceFlowData[LCount]))));
              if (LRequirement <> NullFloat) then
              begin
                LRequirement := ConvertToMcM(LRequirement,FDisplayMonth);
                LSupplyValue := StrToFloat(FormatFloat('00000000000000.000',
                  GetSupplyValueByYear(integer(LReferenceFlowData.Objects[LCount]),FDisplayMonth,AData)));
                LSupplyValue := ConvertToMcM(LSupplyValue,FDisplayMonth);
                LDifference := LRequirement - LSupplyValue;
                if LDifference < 0 then
                  Continue;
                LDifferenceData.Add(FormatFloat('00000000000000.000',LDifference));
              end;
            end;
            for LCount := LDifferenceData.Count-1 downto 0 do
              LSortedDifferenceData.Add(LDifferenceData[LCount]);
            for LCount := 0 to  LSortedDifferenceData.Count-1 do
            begin
              LPercentile := (LCount/LReferenceFlowData.Count)*100;
              LFloatValue := StrToFloat(LSortedDifferenceData[LCount]);
              LDifference := StrToFloat(FormatFloat('00000000000000.000',LFloatValue));
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.AddXY(LPercentile,LDifference);
              ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.AddXY(LPercentile,LDifference);
            end;
          end
          else
          if (FDisplayMonth = 0) then
          begin
            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Color := clRed;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Pen.Width := 2;
            LReferenceFlowData.Clear;
            if not AIFRFeature.GetNodeReferenceFlowData(LUnRankedReferenceFlowData) then
              Exit;
            if LUnRankedReferenceFlowData.Count > 0 then
            begin
              for LMonth := 1 to 12 do
              begin
                LReferenceFlowData.CommaText := LUnRankedReferenceFlowData.CommaText;
                GetMonthData(LReferenceFlowData,LMonth);
                if AData.Count <> LReferenceFlowData.Count then
                  Exit;
                for  LCount := 0 to LReferenceFlowData.Count-1 do
                begin
                  LRequirement := StrToFloat(FormatFloat('00000000000000.000',
                    AIFRFeature.GetRequirementFlowFromReferenceFlow(LMonth,StrToFloat(LReferenceFlowData[LCount]))));
                  LRequirement := ConvertToMcM(LRequirement,LMonth);
                  LSupplyValue := StrToFloat(FormatFloat('00000000000000.000',
                    GetSupplyValueByYear(integer(LReferenceFlowData.Objects[LCount]), LMonth,AData)));
                  LSupplyValue := ConvertToMcM(LSupplyValue,LMonth);
                  LDifference := LRequirement - LSupplyValue;
                  if LDifference < 0 then
                    Continue;
                  LDifferenceData.Add(FormatFloat('00000000000000.000',LDifference));
                end;
              end;
              for LCount := LDifferenceData.Count-1 downto 0 do
                LSortedDifferenceData.Add(LDifferenceData[LCount]);
              for LCount := 0 to  LSortedDifferenceData.Count-1 do
              begin
                LPercentile := (LCount/LReferenceFlowData.Count)*100;
                LFloatValue := StrToFloat(LSortedDifferenceData[LCount]);
                LDifference := StrToFloat(FormatFloat('00000000000000.000',LFloatValue));
                ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.AddXY(LPercentile,LDifference);
              end;
            end
          end;
        end;
      finally
        LReferenceFlowData.Free;
        LDifferenceData.Free;
        LSortedDifferenceData.Free;
        LUnRankedReferenceFlowData.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TOutputComplianceGraphValidator.PopulateDefinedAndSimulatedData(AIFRFeature : TIFRFeature; AData: TStrings);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateDefinedAndSimulatedData';
var
  LCount   : integer;
  LYValue,
  LRelease,
  LPercentile : double;
begin
  try
    ClearChart;
    GetSelectionData;
    if FDisplayMonth = 0 then
      Exit;
    ViewDialog.IFRChannelComplienceGraph.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PrepareChart;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Title.Caption :=
                           FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
    if FUseUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.DefinedAndSimulatedIFR'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUseUnits = ouPerSecond then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.DefinedAndSimulatedIFR'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.SetMinMax(0,100);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Increment := 10;
    ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Color    := clMaroon;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Color    := clMaroon;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Pointer.Style := psCircle;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
    if (AIFRFeature <> nil) then
    begin
      if (FDisplayMonth > 0) then
      begin
        GetSortedDataByMonth(AData,FDisplayMonth);
        for LCount := 0 to AData.Count-1 do
        begin
          if (StrToFloat(AData[LCount]) <> NullFloat) then
          begin
            LPercentile := (LCount/AData.Count)*100;
            LYValue := StrToFloat(AData[LCount]);
            LYValue := ConvertToMcM(LYValue,FDisplayMonth);
            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.AddXY(LPercentile,LYValue,'',clMaroon );
            ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.AddXY(LPercentile,LYValue,'',clMaroon);
            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Title := FAppModules.Language.GetString('OutputReview.OutputGraphSuppliedIFR');
          end;
        end;

        for LCount := 12 downto 1 do
        begin
          LPercentile := AIFRFeature.ExceedencePercentageByIndex[LCount];
          LRelease    := StrToFloat(FormatFloat('00000000000000.000',
                           AIFRFeature.ReleaseByIndexAndMonth[LCount, FDisplayMonth]));
          if (LRelease <> NullFloat) then
          begin
            LRelease := ConvertToMcM(LRelease,FDisplayMonth);
            ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].Visible := True;
            ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].Visible := True;
            ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].Pointer.Style := psCircle;
            ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].Color := clBlue;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].Color := clBlue;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].AddXY(LPercentile,
            LRelease,'',clBlue);
            ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].Title :=
            FAppModules.Language.GetString('OutputReview.OutputGraphDefinedIFR');
            ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].AddXY(LPercentile,
            LRelease,'',clBlue);
          end;
        end;
        DisplayLegend(ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart,FAppModules.Language.GetString('OutputReview.OutputGraphDefinedIFR')
                   ,FAppModules.Language.GetString('OutputReview.OutputGraphSuppliedIFR'));
      end;

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.PopulateDefinedAndRequired(AIFRFeature : TIFRFeature);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateDefinedAndRequired';
var
  LCount   : integer;
  LRequired,
  LRelease,
  LPercentile : double;
  LReferenceData : TStringList;
begin
  try
    ClearChart;
    GetSelectionData;
    if (FDisplayMonth = 0) then
      Exit;
    ViewDialog.IFRChannelComplienceGraph.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PrepareChart;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Title.Caption :=
                           FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
    if FUseUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.DefinedAndSimulatedIFR'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUseUnits = ouPerSecond then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.DefinedAndSimulatedIFR'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);

    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.SetMinMax(0,100);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Increment := 10;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Pointer.Style := psCircle;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
    LReferenceData := TStringList.Create;
    try
      if (AIFRFeature <> nil) then
      begin
        if (FDisplayMonth > 0) then
        begin
          if not (AIFRFeature.GetNodeReferenceFlowData(LReferenceData)) then
            Exit;
          GetSortedDataByMonth(LReferenceData,FDisplayMonth);
          for LCount := 0 to LReferenceData.Count-1 do
          begin
            if (StrToFloat(LReferenceData[LCount]) <> NullFloat) then
            begin
              LPercentile := (LCount/LReferenceData.Count)*100;
              LRequired := AIFRFeature.GetRequirementFlowFromReferenceFlow(FDisplayMonth,StrToFloat(LReferenceData[LCount]));
              LRequired := ConvertToMcM(LRequired,FDisplayMonth);
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.AddXY(LPercentile,LRequired,'',clMaroon );
              ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.AddXY(LPercentile,LRequired,'',clMaroon);
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Title := FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR');
            end;
          end;
          for LCount := 12 downto 1 do
          begin
            LPercentile := AIFRFeature.ExceedencePercentageByIndex[LCount];
            LRelease    := AIFRFeature.ReleaseByIndexAndMonth[LCount, FDisplayMonth];
            if (LRelease <> NullFloat) then
            begin
              LRelease := ConvertToMcM(LRelease,FDisplayMonth);
              ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].Visible := True;
              ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].Visible := True;
              ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].Pointer.Style := psCircle;
              ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].Color := clBlue;
              ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].Color := clBlue;
              ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].AddXY(LPercentile,
              LRelease,'',clBlue);
              ViewDialog.IFRChannelComplienceGraph.LineSeriesArrayOne[FDisplayMonth-1].Title :=
              FAppModules.Language.GetString('OutputReview.OutputGraphDefinedIFR');
              ViewDialog.IFRChannelComplienceGraph.PointSeriesArrayOne[FDisplayMonth-1].AddXY(LPercentile,
              LRelease,'',clBlue);
            end;
          end;
        end;
        DisplayLegend(ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart,FAppModules.Language.GetString('OutputReview.OutputGraphDefinedIFR')
                   ,FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR'));
      end;
      finally
        LReferenceData.Free;
      end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.PopulateIFRHistogram(AIFRFeature: TIFRFeature; AData: TStrings);
const OPNAME ='TOutputComplianceGraphValidator.PopulateIFRHistogram';
var
  LReferenceFlowData : TStringList;
  LUnRankedReferenceFlowData : TStringList;
  LRequirement : double;
  LCount,
  LIndex       : integer;
  LSupply      : double;
  LRequiredAverage : array[1..12] of double;
  LSupplyAverage : array[1..12] of double;
  LMonthName : string;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRChannelComplienceGraph.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PrepareChart;
    if FUseUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('OutputReview.OutputGraphSupplyAndDemand'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUseUnits = ouPerSecond then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('OutputReview.OutputGraphSupplyAndDemand'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Title.Caption  := '';
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Title.Text.Text           := '';
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Increment      := 1;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.RequiredBarSeries.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.ActualBarSeries.Visible   := True;
    if(AIFRFeature = nil) then
      Exit;
    LReferenceFlowData := TStringList.Create;
    LUnRankedReferenceFlowData := TStringList.Create;
    try
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.SetMinMax(0,13);
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Visible := True;
      if not AIFRFeature.GetNodeReferenceFlowData(LUnRankedReferenceFlowData) then
        Exit;

      for LIndex := 1 to 12 do
      begin
        LReferenceFlowData.CommaText := LUnRankedReferenceFlowData.CommaText;
        GetMonthData(LReferenceFlowData,LIndex);
        if AData.Count <> LReferenceFlowData.Count then
          Exit;
        for  LCount := 0 to LReferenceFlowData.Count-1 do
        begin
          LRequirement := AIFRFeature.GetRequirementFlowFromReferenceFlow(LIndex,StrToFloat(LReferenceFlowData[LCount]));
          LSupply := GetSupplyValueByYear(integer(LReferenceFlowData.Objects[LCount]), LIndex,AData);
          LRequiredAverage[LIndex] := LRequiredAverage[LIndex] + LRequirement;
          LSupplyAverage[LIndex] := LSupplyAverage[LIndex] + LSupply;
        end;
        LRequirement := LRequiredAverage[LIndex]/LReferenceFlowData.Count;
        LSupply      := LSupplyAverage[LIndex]/LReferenceFlowData.Count;
        LRequirement := ConvertToMcM(LRequirement,LIndex);
        LSupply      := ConvertToMcM(LSupply,LIndex);
        LMonthName := TYieldModelDataObject(FAppModules.Model.ModelData).
                          CastRunConfigurationData.MonthNameByIndex[LIndex];
        ViewDialog.IFRChannelComplienceGraph.RequiredBarSeries.AddXY(LIndex,LRequirement,LMonthName,clBlue);
        ViewDialog.IFRChannelComplienceGraph.ActualBarSeries.AddXY(LIndex,LSupply,LMonthName,clRed);
      end;
      GetChartLegend(ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart);
    finally
      LReferenceFlowData.Free;
      LUnRankedReferenceFlowData.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.GetChartLegend(AChart: TFieldChart);
const OPNAME = 'TOutputComplianceGraphValidator.GetChartLegend';
const C_Values : array [0..1] of string = ('Supply','Demand');
var
  LIndex : integer;
  LCount : integer;
begin
  try
    LCount := 0;
    for LIndex := 0 to AChart.SeriesCount-1 do
    begin
       if (AChart.Series[LIndex] is TBarSeries) then
       begin
         if LCount <= 1 then
         begin
           AChart.Series[LIndex].ShowInLegend := True;
           AChart.Series[LIndex].Title        := C_Values[LCount];
           LCount := LCount+1;
         end
         else
           AChart.Series[LIndex].ShowInLegend := False;
       end
       else
         AChart.Series[LIndex].ShowInLegend := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.GetSupplyByMonthYear(ASupplyData : TStrings;AMonth,AYear : integer) : double;
const OPNAME = 'TOutputComplianceGraphValidator.GetSupplyByMonthYear';
var
  LMonthStr : string;
  LIndex : integer;
  LYear : integer;
  LStrValue : string;
begin
  Result := 0;
  try
    if (ASupplyData <> nil) and (AMonth > 0) and (AYear > 0) then
    begin
      for LIndex := 0 to ASupplyData.Count-1 do
      begin
        LStrValue := ASupplyData.Strings[LIndex];
        LMonthStr := Copy(LStrValue,Pos('_',LStrValue)+1,Length(LStrValue));
        if Trim(LMonthStr) <> '' then
        begin
          LYear := integer(ASupplyData.Objects[LIndex]);
          if (AMonth = StrToInt(LMonthStr)) and (AYear = LYear) then
            Result := StrToFloat(Copy(LStrValue,1,Pos('_',LStrValue)-1));
        end;
      end;
    end;
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.GetMonthData(AData: TStrings;AMonth : integer);
const OPNAME = 'TOutputComplianceGraphValidator.GetMonthData';
var
  LMonthData,
  LLineData   : TStringList;
  LIndex : integer;
  LYear : integer;
begin
  try
    LMonthData         := TStringList.Create;
    LLineData          := TStringList.Create;
    try
      LMonthData.Duplicates := dupAccept;
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData.Strings[LIndex];
        LYear           := StrToInt(LLineData[0]);
        LMonthData.AddObject(FormatFloat('00000000000000.000',StrToFloat(LLineData[AMonth])),TObject(LYear));
      end;
      AData.Clear;
      for LIndex := 0 to LMonthData.Count-1 do
      begin
        LYear := integer(LMonthData.Objects[LIndex]);
        AData.AddObject(LMonthData[LIndex], TObject(LYear));
      end;
     finally
       FreeAndNil(LMonthData);
       FreeAndNil(LLineData);
     end;
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.PopulateReservoirStorageChartData(AData: TStrings);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateReservoirStorageChartData';
var
  LMonthNumbers  : array[0..11] of integer;
  LYear,
  LStartMonth,
  LIndex,
  LCount: integer;
  LMonthlyValues: TStringList;
  LMonthlyValue,
  LFullStorageVol,
  LDeadStorageVol  : double;
  LDate: TDateTime;
  LMin,
  LMax,
  LPos : double;
  LReservoirData : IReservoirData;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.ReservoirStorageGraph.Visible := True;
    if(AData.Count > 0) then
    begin
      LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      LFullStorageVol := LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
      LDeadStorageVol := LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
      LStartMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.StartMonthNumber-1;

      for LIndex := 0 to 11 do
      begin
        LStartMonth := LStartMonth + 1;
        if(LStartMonth > 12) then
           LStartMonth := 1;
        LMonthNumbers[LIndex] := LStartMonth;
      end;

      LMonthlyValues := TStringList.Create;
      try
        for LIndex := 0 to AData.Count -1 do
        begin
          LMonthlyValues.CommaText := AData[LIndex];
          LYear := StrToInt(LMonthlyValues[0]);
          for LCount := 1 to LMonthlyValues.Count -2 do
          begin
            LDate := EncodeDate(LYear,LMonthNumbers[LCount-1],1);
            LMonthlyValue := StrToFloat(LMonthlyValues[LCount]);
            ViewDialog.ReservoirStorageGraph.ReservoirActTimeSeries.AddXY(LDate,LMonthlyValue);
            ViewDialog.ReservoirStorageGraph.ReservoirSimTimeSeries[LCount-1].Add(LMonthlyValue);
            if(LMonthNumbers[LCount-1] = 12) then
              LYear := LYear + 1;
          end;
        end;
        ViewDialog.ReservoirStorageGraph.ReservoirActTimeSeries.Active := True;
        LMin := ViewDialog.ReservoirStorageGraph.ReservoirActTimeSeries.XValues.MinValue;
        LMax := ViewDialog.ReservoirStorageGraph.ReservoirActTimeSeries.XValues.MaxValue;
        for LIndex := 0 to 11 do
        begin
          LPos := LMin + (((LMax - LMin) / 12) * (LIndex + 0.5));
          ViewDialog.ReservoirStorageGraph.ReservoirSimTimeSeries[LIndex].Position := LPos;
          ViewDialog.ReservoirStorageGraph.ReservoirSimTimeSeries[LIndex].Active := True;
        end;
        ViewDialog.ReservoirStorageGraph.ReservoirFSVSeries.AddXY(LMin, LFullStorageVol);
        ViewDialog.ReservoirStorageGraph.ReservoirFSVSeries.AddXY(LMax, LFullStorageVol);
        ViewDialog.ReservoirStorageGraph.ReservoirFSVSeries.Active := True;

        ViewDialog.ReservoirStorageGraph.ReservoirDSVSeries.AddXY(LMin, LDeadStorageVol);
        ViewDialog.ReservoirStorageGraph.ReservoirDSVSeries.AddXY(LMax, LDeadStorageVol);
        ViewDialog.ReservoirStorageGraph.ReservoirDSVSeries.Active := True;
      finally
        LMonthlyValues.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.DrawColorBands;
const OPNAME = 'TOutputComplianceGraphValidator.DrawColorBands';
var
  LIndex,
  LIndex2,
  LCount,
  LValCount : integer;
  LReservoirData : IReservoirData;
  LFSLevel,
  LDSLevel,
  LBOReservoir,
  LXValue,
  LYValue : double;
  LOpLevels : array of double;
  LYValList,
  LXValList  : TChartValueList;
  LFSListValues,
  LDSListValues,
  LBRListValues,
  LTotValues : TStringList;
  LOPListValues : array of TStringList;
begin
  try
    if (NetworkElementType <> votWetland) then
    begin
      LValCount := ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationTimeSeries.YValues.Count;
      LYValList  := ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationTimeSeries.YValues;
      LXValList  := ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationTimeSeries.XValues;
      LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirByIdentifier[FIdentifier];
      LCount := LReservoirData.ReservoirZoneElevationsData.ReservoirDrawDownLevelsCount;

      SetLength(LOpLevels, LCount);
      SetLength(LOPListValues, LCount);

      for LIndex := 0 to LCount - 1 do
        LOPListValues[LIndex] := TStringList.Create;

      for LIndex := 0 to LCount - 1 do
        LOpLevels[LIndex] := LReservoirData.ReservoirZoneElevationsData.DrawDownLevelByIndex[LIndex].AverageElevations;

      LFSLevel     := LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
      LDSLevel     := LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;
      LBOReservoir := LReservoirData.ReservoirZoneElevationsData.BottomOfReservoir.Elevation;

      LFSListValues := TStringList.Create;
      LDSListValues := TStringList.Create;
      LBRListValues := TStringList.Create;
      LTotValues    := TStringList.Create;
      try
        for LIndex := 0 to LValCount - 1 do
        begin
          if (LYValList[LIndex] <= LFSLevel) and
             (LYValList[LIndex] >= LOpLevels[0]) then
            LFSListValues.AddObject(FloatToStr(LXValList[LIndex]),
                                    TObject(ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationFSLSeries.Color))
          else
            LFSListValues.AddObject('',nil);
        end;

        for LIndex := 0 to Length(LOpLevels) - 1 do
        begin
          for LIndex2 := 0 to LValCount - 1 do
          begin
            if (LIndex = (Length(LOpLevels) - 1)) then
            begin
              if (LYValList[LIndex2] <= LOpLevels[LIndex]) and
                 (LYValList[LIndex2] >= LDSLevel) then
                LOPListValues[LIndex].AddObject(FloatToStr(LXValList[LIndex2]),
                                      TObject(ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationOPLSeries[LIndex].Color))
              else
                LOPListValues[LIndex].AddObject('',nil);
            end
            else
            begin
              if (LYValList[LIndex2] <= LOpLevels[LIndex]) and
                 (LYValList[LIndex2] >= LOpLevels[LIndex + 1]) then
                LOPListValues[LIndex].AddObject(FloatToStr(LXValList[LIndex2]),
                                      TObject(ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationOPLSeries[LIndex].Color))
              else
                LOPListValues[LIndex].AddObject('',nil);
            end;
          end;
        end;

        for LIndex := 0 to LValCount - 1 do
        begin
          if (LYValList[LIndex] <= LDSLevel) and
             (LYValList[LIndex] >= LBOReservoir) then
            LDSListValues.AddObject(FloatToStr(LXValList[LIndex]),
                                    TObject(ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationDSLSeries.Color))
          else
            LDSListValues.AddObject('',nil);
        end;
        for LIndex := 0 to LValCount - 1 do
        begin
          if (LYValList[LIndex] <= LBOReservoir) and
             (LYValList[LIndex] >= 0) then
            LBRListValues.AddObject(FloatToStr(LXValList[LIndex]),
                                    TObject(ViewDialog.ReservoirChannelComparitor.ReservoirZoneElevationBORSeries.Color))
          else
            LBRListValues.AddObject('',nil);
        end;

        for LIndex := 0 to LValCount - 1 do
          LTotValues.AddObject('', nil);

        for LIndex := 0 to LValCount - 1 do
          if (LFSListValues[LIndex] <> '') and (LFSListValues.Objects[LIndex] <> nil) then
          begin
            LTotValues[LIndex]         := LFSListValues[LIndex];
            LTotValues.Objects[LIndex] := LFSListValues.Objects[LIndex];
          end;

        for LIndex := 0 to Length(LOPListValues) - 1 do
          for LIndex2 := 0 to LValCount - 1 do
            if (LOPListValues[LIndex].Strings[LIndex2] <> '') and (LOPListValues[LIndex].Objects[LIndex2] <> nil) then
            begin
              LTotValues[LIndex2]         := LOPListValues[LIndex].Strings[LIndex2];
              LTotValues.Objects[LIndex2] := LOPListValues[LIndex].Objects[LIndex2];
            end;

        for LIndex := 0 to LValCount - 1 do
          if (LDSListValues[LIndex] <> '') and (LDSListValues.Objects[LIndex] <> nil) then
          begin
            LTotValues[LIndex]         := LDSListValues[LIndex];
            LTotValues.Objects[LIndex] := LDSListValues.Objects[LIndex];
          end;

        for LIndex := 0 to LValCount - 1 do
          if (LBRListValues[LIndex] <> '') and (LBRListValues.Objects[LIndex] <> nil) then
          begin
            LTotValues[LIndex]         := LBRListValues[LIndex];
            LTotValues.Objects[LIndex] := LBRListValues.Objects[LIndex];
          end;

        if (LTotValues.Count <> 0) then
        begin
          LYValue := ViewDialog.ReservoirChannelComparitor.ChannelChart.LeftAxis.Maximum;
          LYValue := LYValue / 2;
          for LIndex := 0 to LValCount - 1 do
          begin
            if (LTotValues[LIndex] <> '') then
            begin
              LXValue := StrToFloat(LTotValues[LIndex]);
              ViewDialog.ReservoirChannelComparitor.ColorBands.AddXY(LXValue,
                                                                     LYValue,
                                                                     '',
                                                                     TColor(LTotValues.Objects[LIndex]));
            end;
          end;

          ViewDialog.ReservoirChannelComparitor.ColorBands.Active := True;
        end;
      finally
        LFSListValues.Free;
        LDSListValues.Free;
        LBRListValues.Free;
        LTotValues.Free;
        for LIndex := 0 to Length(LOpLevels) - 1 do
          LOPListValues[LIndex].Free;
        SetLength(LOPListValues, 0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.PopulateDamLevelSeries(ALineSeries: TLineSeries; ADemandFileName: string;
           AMinX,AMaxX: TDateTime);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateDamLevelSeries';
var
  LDataSet : TAbstractModelDataset;
  lFieldName,
  LSQL     : string;
  LYear    : integer;
  LMonth   : integer;
  LValue   : double;
  LDate    : TDateTime;

begin
  try
    ALineSeries.Clear;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQL := 'SELECT YearValue as [Year],'+
              'MonthValue01 as Value01,'+
              'MonthValue02 as Value02,'+
              'MonthValue03 as Value03,'+
              'MonthValue04 as Value04,'+
              'MonthValue05 as Value05,'+
              'MonthValue06 as Value06,'+
              'MonthValue07 as Value07,'+
              'MonthValue08 as Value08,'+
              'MonthValue09 as Value09,'+
              'MonthValue10 as Value10,'+
              'MonthValue11 as Value11,'+
              'MonthValue12 as Value12 '+
              ' FROM HistoricDamLevels A WHERE ' +
               GetScenarioWhereClause +
              ' AND FileName = '+QuotedStr(ADemandFileName);
      LDataSet.DataSet.Close;
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      if not(LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
      begin
        while not LDataSet.DataSet.Eof do
        begin
          LYear := LDataSet.DataSet.FieldByName('Year').AsInteger;
          for LMonth := 1 to 12 do
          begin
            LFieldName    := Format('%s%2.2d',['Value',LMonth]);
            if not LDataSet.DataSet.FieldByName(lFieldName).IsNull then
            begin
              LValue := LDataSet.DataSet.FieldByName(lFieldName).AsFloat;
              LDate  := EncodeDate(LYear,LMonth,1);
              if(LDate >= AMinX) and (LDate <= AMaxX) then
                ALineSeries.AddXY(LDate,LValue);
            end;
          end;
          LDataSet.DataSet.Next;
        end;
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGraphValidator.GetScenarioWhereClause: string;
const OPNAME = 'TOutputComplianceGraphValidator.GetScenarioWhereClause';
begin
  Result := '';
  try
    Result :=
      ' (A.Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)     + ') AND ' +
      ' (A.StudyAreaName = ' + QuotedStr(FAppModules.StudyArea.StudyAreaCode) + ') AND ' +
      ' (A.SubArea       = ' + QuotedStr(FAppModules.StudyArea.SubAreaCode)   + ') AND ' +
      ' (A.Scenario      = ' + QuotedStr(FAppModules.StudyArea.ScenarioCode)  + ') ';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputComplianceGraphValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdComplianceGraph,FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.GetSelectionData;
const OPNAME = 'TOutputComplianceGraphValidator.GetSelectionData';
var
  LDataSelection : IOutputDataSelection;
begin
  try
    LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
    if (LDataSelection <> nil) then
    begin
      FUseUnits     := LDataSelection.Units;
      FDisplayMonth := LDataSelection.DisplayMonth;
      FValueType    := LDataSelection.ValueType;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.GetSortedAllData(AData: TStrings);
const OPNAME = 'TOutputComplianceGraphValidator.GetSortedAllData';
var
  LMonthData,
  LMonthlySortedData,
  LLineData   : TStringList;
  LIndex : integer;
  LStringValue : string;
  LFloatValue : double;
  LYear : integer;
  LMonth : integer;
  LFloatStr,
  LMonthStr : string;
begin
  try
    LMonthData         := TStringList.Create;
    LLineData          := TStringList.Create;
    LMonthlySortedData := TStringList.Create;
    try
      LMonthData.Sorted := True;
      LMonthData.Duplicates := dupAccept;
      LMonthlySortedData.Sorted := True;
      LMonthlySortedData.Duplicates := dupAccept;
      for LIndex := 0 to AData.Count-1 do
      begin
        LMonth := 0;
        LFloatValue := 0;
        LLineData.CommaText := AData.Commatext;
        LStringValue    := LLineData[LIndex];
        LMonthStr       := Copy(LStringValue,Pos('_',LStringValue)+1,Length(LStringValue));
        if Trim(LMonthStr) <> '' then
          LMonth := StrToInt(LMonthStr);
        LYear           := integer(AData.Objects[LIndex]);
        LFloatStr := Copy(LStringValue,1,Pos('_',LStringValue)-1);
        if (Trim(LFloatStr) <> '') then
          LFloatValue     := StrToFloat(LFloatStr);
        LMonthData.AddObject(FormatFloat('00000000000000.000',LFloatValue),TObject(LYear));
        LMonthlySortedData.AddObject(FormatFloat('00000000000000.000',LFloatValue),TObject(LMonth));
      end;
      AData.Clear;
      for LIndex := LMonthData.Count-1 downto 0 do
      begin
          LYear := integer(LMonthData.Objects[LIndex]);
          LMonth := integer(LMonthlySortedData.Objects[LIndex]);
          AData.AddObject(LMonthData[LIndex], TObject(LYear));
          FMonthlyData.AddObject(LMonthlySortedData[LIndex], TObject(LMonth));
      end;
     finally
       FreeAndNil(LMonthData);
       FreeAndNil(LLineData);
       FreeAndNil(LMonthlySortedData);
     end;
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TOutputComplianceGraphValidator.GetSortedDataByMonth(AData: TStrings;AMonth : integer);
const OPNAME = 'TOutputComplianceGraphValidator.GetSortedDataByMonth';
var
  LMonthData,
  LLineData   : TStringList;
  LIndex : integer;
  LStringValue : string;
  LFloatValue : double;
  LYear : integer;
begin
  try
    LMonthData         := TStringList.Create;
    LLineData          := TStringList.Create;
    try
      LMonthData.Sorted := True;
      LMonthData.Duplicates := dupAccept;
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData.Strings[LIndex];
        LStringValue    := LLineData[AMonth];
        LYear           := StrToInt(LLineData[0]);
        LFloatValue     := StrToFloat(LStringValue);
        LMonthData.AddObject(FormatFloat('00000000000000.000',LFloatValue),TObject(LYear));
      end;
      AData.Clear;
      for LIndex := LMonthData.Count-1 downto 0 do
      begin
        LYear := integer(LMonthData.Objects[LIndex]);
        AData.AddObject(LMonthData[LIndex], TObject(LYear));
      end;
     finally
       FreeAndNil(LMonthData);
       FreeAndNil(LLineData);
     end;
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComplianceGraphValidator.GetSupplyValueByYear(ASupplyYear : integer;AMonth : integer;AData: TStrings) : double;
const OPNAME = 'TOutputComplianceGraphValidator.GetSupplyValueByYear';
var
  LIndex : integer;
  LSupplyData : TStringList;
begin
  Result := NullFloat;
  try
    LSupplyData := TStringList.Create;
    try
      for LIndex := 0 to AData.Count-1 do
      begin
        LSupplyData.CommaText := AData[LIndex];
        if ASupplyYear = StrToInt(LSupplyData[0]) then
        begin
          Result := StrToFloat(LSupplyData[AMonth]);
          Break;
        end;
      end;
    finally
      LSupplyData.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.PopulateIFRRequirementAndSupply(AIFRFeature : TIFRFeature; AData: TStrings);
const OPNAME = 'TOutputComplianceGraphValidator.PopulateIFRRequirementAndSupply';
var
  LReferenceFlowData : TStringList;
  LSortedReferenceFlowData : TStringList;
  LRequirement,
  LPercentile: double;
  LIndex : integer;
  LSupply : double;
  LYear,
  LMonth : integer;
  LSupplyData,
  LMonthData : TStringList;
begin
  try
    ClearChart;
    GetSelectionData;
    ViewDialog.IFRChannelComplienceGraph.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PrepareChart;
    if FUseUnits = ouMcmPerMonthOrYear then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRAndSupply'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if FUseUnits = ouPerSecond then
      ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRAndSupply'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Legend.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Title.Caption :=
                   FAppModules.Language.GetString('TIFRSiteDialog.ExceedenceProbability');
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.SetMinMax(0,100);
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.BottomAxis.Increment := 10;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Pointer.Style := psCircle;
    ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesTwo.Visible := True;
    ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart.Title.Text.Text := GetMonthDescription(FDisplayMonth);
    ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Color   := clMaroon;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Color   := clMaroon;
    ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Pointer.Style   := psCircle;

    ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Color   := clRed;
    if(AIFRFeature = nil) then
      Exit;
    LReferenceFlowData := TStringList.Create;
    LSortedReferenceFlowData := TStringList.Create;
    LMonthData := TStringList.Create;
    LSupplyData := TStringList.Create;
    try
      if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
        Exit;
      if (FDisplayMonth > 0) then
      begin
        LSortedReferenceFlowData.CommaText := LReferenceFlowData.CommaText;
        GetSortedDataByMonth(LSortedReferenceFlowData, FDisplayMonth);
        for LIndex := 0 to LSortedReferenceFlowData.Count-1 do
        begin
          LPercentile  := (LIndex/LReferenceFlowData.Count)*100;
          LRequirement := StrToFloat(FormatFloat('00000000000000.000',
             AIFRFeature.GetRequirementFlowFromReferenceFlow(FDisplayMonth,StrToFloat(LSortedReferenceFlowData[LIndex]))));
          if(LRequirement <> NullFloat) then
          begin
            LRequirement := ConvertToMcM(LRequirement,FDisplayMonth);
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Pen.Width   := 2;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Color := clMaroon;
            ViewDialog.IFRChannelComplienceGraph.PointSeriesTwo.Color := clMaroon;
            ViewDialog.IFRChannelComplienceGraph.PointSeriesTwo.Pointer.Style := psCircle;
            ViewDialog.IFRChannelComplienceGraph.PointSeriesTwo.ShowInLegend := False;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.AddXY(LPercentile,LRequirement,'',clMaroon);
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.ShowInLegend := True;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Title := FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR');
            ViewDialog.IFRChannelComplienceGraph.PointSeriesTwo.AddXY(LPercentile,LRequirement,'',clMaroon);
            ViewDialog.IFRChannelComplienceGraph.PointSeriesTwo.ShowInLegend := False;
            LSupply := StrToFloat(FormatFloat('00000000000000.000',
                GetSupplyValueByYear(integer(LSortedReferenceFlowData.Objects[LIndex]),FDisplayMonth,AData)));
            if LSupply <> NullFloat then
            begin
              LSupply := ConvertToMcM(LSupply,FDisplayMonth);
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.AddXY(LPercentile,LSupply,'',clRed);
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.ShowInLegend := True;
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Title := FAppModules.Language.GetString('OutputReview.OutputGraphSuppliedIFR');
              ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Pointer.Style := psCircle;
              ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.Color := clRed;
              ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.AddXY(LPercentile,LSupply,'',clRed);
              ViewDialog.IFRChannelComplienceGraph.PointSeriesOne.ShowInLegend := False;
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Pen.Width   := 2;
              ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Color := clRed;
            end;
          end;
        end;
      end
      else
      if (FDisplayMonth = 0) then
      begin
        ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Color   := clMaroon;
        ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Color   := clRed;
        ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Pen.Width   := 2;
        ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Pen.Width   := 2;
        for LMonth := 1 to 12 do
        begin
          LReferenceFlowData.Clear;
          if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then
            Exit;
          GetMonthData(LReferenceFlowData,LMonth);
          if AData.Count <> LReferenceFlowData.Count then
            Exit;
          for LIndex := 0 to LReferenceFlowData.Count-1 do
          begin
            LRequirement :=StrToFloat(FormatFloat('00000000000000.000',
               AIFRFeature.GetRequirementFlowFromReferenceFlow(LMonth,StrToFloat(LReferenceFlowData[LIndex]))));
            LRequirement := ConvertToMcM(LRequirement,LMonth);
            LMonthData.AddObject(FloatToStr(LRequirement)+'_'+IntToStr(LMonth),TObject(LReferenceFlowData.Objects[LIndex]));
            LSupply      := StrToFloat(FormatFloat('00000000000000.000',
               GetSupplyValueByYear(integer(LMonthData.Objects[LIndex]),LMonth,AData)));
            LYear := integer(LMonthData.Objects[LIndex]);
            LSupply := ConvertToMcM(LSupply,LMonth);
            LSupplyData.AddObject(FloatToStr(LSupply)+'_'+IntToStr(LMonth),TObject(LYear));
          end;
        end;
        FMonthlyData.Clear;
        GetSortedAllData(LMonthData);
        for LIndex := 0 to LMonthData.Count-1 do
        begin
          LPercentile  := (LIndex/LMonthData.Count)*100;
          LRequirement := StrToFloat(LMonthData[LIndex]);
          LYear := integer(LMonthData.Objects[LIndex]) ;
          LMonth := integer(FMonthlyData.Objects[LIndex]) ;
          LSupply      := GetSupplyByMonthYear(LSupplyData,LMonth,LYear);
          if(LRequirement <> NullFloat) and (LSupply <> NullFloat) then
          begin
            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.AddXY(LPercentile,LRequirement,'',clMaroon);
            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.Title := FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR');
            ViewDialog.IFRChannelComplienceGraph.LineSeriesOne.ShowInLegend := True;
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.AddXY(LPercentile,LSupply,'',clRed);
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.Title := FAppModules.Language.GetString('OutputReview.OutputGraphSuppliedIFR');
            ViewDialog.IFRChannelComplienceGraph.LineSeriesTwo.ShowInLegend := True;
          end;
        end;
      end;
      DisplayLegend(ViewDialog.IFRChannelComplienceGraph.RequirementFlowValuesChart,FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR')
                    ,FAppModules.Language.GetString('OutputReview.OutputGraphSuppliedIFR'));
    finally
      LReferenceFlowData.Free;
      LSortedReferenceFlowData.Free;
      LMonthData.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComplianceGraphValidator.ConvertToMcM(AValue : double; AIndex : integer) : double;
const OPNAME = 'TOutputComplianceGraphValidator.ConvertToMcM';
var
  LDaysInMonth : double;
begin
  Result :=  AValue;
  try
    LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[AIndex];
    case FUseUnits of
      ouMcmPerMonthOrYear :
      Result := (AValue*LDaysInMonth*24*60*60)/Power(10,6);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComplianceGraphValidator.GetMonthDescription(AMonthIndex: integer): string;
const OPNAME = 'TOutputComplianceGraphValidator.GetMonthDescription';
const C_MonthsDescr : array [0..12] of string = ('ALL','OCTOBER','NOVEMBER','DECEMBER','JANUARY','FEBRUARY'
                                                 ,'MARCH','APRIL','MAY','JUNE','JULY','AUGUST','SEPTEMBER');
begin
  Result := '';
  try
    Result := C_MonthsDescr[AMonthIndex];
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGraphValidator.GetSortedDataForAllMonths(AData: TStrings);
const OPNAME = 'TOutputComplianceGraphValidator.GetSortedDataForAllMonths';
var
  LMonthData,
  LLineData   : TStringList;
  LIndex : integer;
  LStringValue : string;
  LFloatValue : double;
begin
  try
    LMonthData         := TStringList.Create;
    LLineData          := TStringList.Create;
    try
      LMonthData.Sorted     := True;
      LMonthData.Duplicates := dupAccept;
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData.Strings[LIndex];
        LStringValue        := LLineData[0];
        LFloatValue         := StrToFloat(LStringValue);
        LMonthData.Add(FormatFloat('00000000000000.000',LFloatValue));
      end;
      AData.Clear;
      for LIndex := LMonthData.Count-1 downto 0 do
        AData.Add(LMonthData[LIndex]);
     finally
       FreeAndNil(LMonthData);
       FreeAndNil(LLineData);
     end;
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGraphValidator.DisplayLegend(AChart: TFieldChart;ASeriesA,ASeriesB : string);
const OPNAME = 'TOutputComplianceGraphValidator.DisplayLegend';
var
  LIndex : integer;
begin
  try
    if Assigned(AChart) then
    begin
      for LIndex := 0 to AChart.SeriesCount-1 do
      begin
        if (AChart.Series[LIndex] is TLineSeries) and
           ((AChart.Series[LIndex].Title = ASeriesA) or
           (AChart.Series[LIndex].Title = ASeriesB)) then
          AChart.Series[LIndex].ShowInLegend := True
        else
          AChart.Series[LIndex].ShowInLegend := False;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.


