//
//
//  UNIT      : Contains the class TOutputComplianceGridValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputComplianceGridValidator;

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
  VoaimsCom_TLB,
  UDataEditComponent,
  UIFRFeatures,
  UYieldContextValidationType,
  UOutputComplianceGridDialog;

type
  TOutputComplianceGridValidator = class(TAbstractOutputDialogValidator)
  protected
    FCurrentViewData     : TOutputDataType;
    FPrevViewData        : TOutputDataType;
    FLoadCase            : integer;
    FSequence            : integer;
    FMonth               : integer;
    FUnits               : TOutputUnits;
    FValueType           : TOutputValueType;
    FTimeStep            : TOutputTimeStep;
    FHighLight           : WordBool;
    FDisplayMonth        : integer;

    procedure CreateMemberObjects; override;

    procedure OnViewDataTypeChange(Sender: TObject);
    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure PopulateDialogSelectors;
    procedure RePopulateDataViewer;
    procedure PopulateDemandSupplyChannels(AChannel : IGeneralFlowChannel;AData: TStrings);
    procedure PopulateData(AChannelDemandValues,AData: TStrings);
    procedure PopulateStats(AChannelDemandValues,AData: TStrings);
    procedure PopulateDemandSupplyValues(AData: TStrings;AChannel : IGeneralFlowChannel);

    procedure PopulateMonthlyDeficitGridData(AData,ADeficts: TStrings);

    procedure PopulateSupplyDeficitGridData(AData: TStrings);
    procedure PopulateSupplyDeficitAggregateGridData(AData: TStrings);
    procedure PopulateIFRStats(AIFRFeature : TIFRFeature;AData : TStrings);
    procedure PopulateIFRData(AIFRFeature : TIFRFeature;AData : TStrings);
    procedure PopulateIFRMonthlyDeficitGridData(AIFRFeature : TIFRFeature;AData : TStrings);
    procedure ClearGrid;

    procedure GetSelectionData;
    procedure ChangeViewDataLabel;
    procedure SetCurrentViewData;
    procedure GetMonthData(AData: TStrings;AMonth : integer);
    function GetSupplyValueByYear(ASupplyYear : integer;AMonth : integer;AData: TStrings) : double;
    procedure PopulateChannelDemandData(AValues: TStringList; AChannel: IGeneralFlowChannel);


  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    function GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    function GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ViewDialog: TOutputComplianceGridDialog;overload;
  end;

implementation

uses
  VCLTee.Series,
  Windows,
  SysUtils,
  VCL.Graphics,
  UConstants,
  UOutputData,
  UDataSetType,
  UOutputGridValidator,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, URunConfigurationData, VCL.Grids;

{ TOutputComplianceGridValidator }

procedure TOutputComplianceGridValidator.CreateMemberObjects;
const OPNAME = 'TOutputComplianceGridValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputComplianceGridDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.cmbViewDataType.OnSelect:= OnViewDataTypeChange;
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.Initialise: boolean;
const OPNAME = 'TOutputComplianceGridValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    FPrevViewData    := btNone;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputComplianceGridValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'LOADCASESCOUNT') OR
       (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  OR
       (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputComplianceGridValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComplianceGridValidator.LanguageHasChanged';
begin
  Result := False;
  try
    if(NetworkElementType in [votMasterControl,votChannel]) then
      TabShetCaption := 'Deficit'
    else
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputComplianceGrid');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.SaveState: boolean;
const OPNAME = 'TOutputComplianceGridValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.ViewDialog : TOutputComplianceGridDialog;
const OPNAME = 'TOutputComplianceGridValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputComplianceGridDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputComplianceGridValidator.GetNextSignificantRecord';
begin
  Result := 0;
  try
    Result := ACurrentRecord  + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputComplianceGridValidator.GetPreviousSignificantRecord';
begin
  Result := 0;
  try
    Result := ACurrentRecord  - 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.ClearDataViewer;
const OPNAME = 'TOutputComplianceGridValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearGrid;
    ViewDialog.cmbViewDataType.Items.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.PopulateDataViewer;
const OPNAME = 'TOutputComplianceGridValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputComplianceGridValidator.PopulateDialogSelectors';
var
  LViewData : string;
  lIndex,
  lSelectIndex : integer;
  LChannel : IGeneralFlowChannel;
begin
  try
    if (FIdentifier >= 0)  and (NetworkElementType <> votNone)then
    begin
      case NetworkElementType of
        votMasterControl:
        begin
          LViewData := FAppModules.Language.GetString('OutputReview.OutputCompGridMonthlyDeficits');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btChannelFlowDeficit)));
          ViewDialog.grdMonthlySupplyData.Heading := LViewData;
          LViewData := FAppModules.Language.GetString('OutputReview.DemandandSupplyData');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRData)));

          LViewData := FAppModules.Language.GetString('OutputReview.DemandandSupplyStats');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRStats)));
          {LViewData := FAppModules.Language.GetString('OutputReview.OutputCompGridSupplyDeficits');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));
          LViewData := FAppModules.Language.GetString('OutputReview.OutputCompGridSupplyComplianceAggregate');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));}
        end;
        votReservoir:
        begin
        end;
        votNodeWithInflow:
        begin
        end;
        votNodeWithoutInflow:
        begin
        end;
        votChannel:
        begin
          LViewData := FAppModules.Language.GetString('OutputReview.OutputCompGridMonthlyDeficits');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btChannelFlowDeficit)));
          ViewDialog.grdMonthlySupplyData.Heading := LViewData;
          {LViewData := FAppModules.Language.GetString('OutputReview.OutputCompGridSupplyDeficits');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btChannelSuppAndDeficitPerc)));
          LViewData := FAppModules.Language.GetString('OutputReview.OutputCompGridSupplyComplianceAggregate');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btChannelSuppAndCompliencePerc)));}
          LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
          if (LChannel <> nil) then
          begin

            if((LChannel.ChannelType = 8)) or
              (LChannel.ChannelType = 2) or (LChannel.ChannelType = 11) or
              (LChannel.ChannelType = 14) then
            begin

              LViewData := FAppModules.Language.GetString('OutputReview.DemandandSupplyData');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRData)));

              LViewData := FAppModules.Language.GetString('OutputReview.DemandandSupplyStats');
              ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btIFRStats)));
            end;

          end;
        end;
        votChannelArea:
        begin
          LViewData := FAppModules.Language.GetString('OutputReview.OutputCompGridMonthlyDeficits');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btChannelFlowDeficit)));
          ViewDialog.grdMonthlySupplyData.Heading := LViewData;
        end;
        votIrrigationArea:
        begin
        end;
        votPowerPlant:
        begin
        end;
      end;
{      if(ViewDialog.cmbViewDataType.Items.Count > 0) then
      begin
        ViewDialog.cmbViewDataType.ItemIndex := 0;
        SetCurrentViewData; }
      if (ViewDialog.cmbViewDataType.Items.Count > 0) then
      begin
//        ViewDialog.cmbViewDataType.ItemIndex := 0;
        lSelectIndex := -1;
        if (FPrevViewData <> btNone) then
        begin
          for lIndex := 0 to ViewDialog.cmbViewDataType.Items.Count - 1 do
          begin
            if (Integer(FPrevViewData) = Integer(ViewDialog.cmbViewDataType.Items.Objects[lIndex])) then
            begin
              lSelectIndex := lIndex;
              Break;
            end;
          end;
        end;
        if (lSelectIndex >= 0) then
          ViewDialog.cmbViewDataType.ItemIndex := lSelectIndex
        else
        if (ViewDialog.cmbViewDataType.Items.Count > 0) then
          ViewDialog.cmbViewDataType.ItemIndex := 0;
        SetCurrentViewData;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.RePopulateDataViewer;
const OPNAME = 'TOutputComplianceGridValidator.RePopulateDataViewer';
var
  LDataContainer              : TStringList;
  LSupplyDeficitData          : TStringList;
  LSupplyDeficitAggregateData : TStringList;
  LErrors                     : string;
  LIFRFeature                 : TIFRFeature;
  LChannel                    : IGeneralFlowChannel;
begin
  try
    ClearGrid;
    GetSelectionData;
    ChangeViewDataLabel;
    if(FIdentifier      >= 0) and
      (NetworkElementType   <> votNone) and
      (FCurrentViewData <> btNone) then
    begin
      LDataContainer              := TStringList.Create;
      LSupplyDeficitData          := TStringList.Create;
      LSupplyDeficitAggregateData := TStringList.Create;
      try
        LErrors := '';
        ViewDialog.ShowError(LErrors);
        case FCurrentViewData of
          btChannelFlowDeficit:
          begin
            if(NetworkElementType = votChannelArea) then
            begin
              if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.
                 GetChannelAreaComplianceGridData(LDataContainer,LSupplyDeficitData,FLoadCase,FSequence,FIdentifier,LErrors)) then
              begin
                PopulateMonthlyDeficitGridData(LDataContainer,LSupplyDeficitData);
                ViewDialog.grdMonthlySupplyData.Visible         := True;
                ViewDialog.grdSupplyCompliance.Visible          := False;
                ViewDialog.grdSupplyComplianceAggregate.Visible := False;
                ViewDialog.grdIFRStatsComplianceData.Visible    := False;
              end
              else
                ViewDialog.ShowError(LErrors);
            end
            else
            begin
              {LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                               CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
              if (LIFRFeature <> nil) then
              begin
                if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
                  LDataContainer,btMonthlyAverageChannelFlow,FIdentifier,LErrors)) then
                begin
                  PopulateIFRMonthlyDeficitGridData(LIFRFeature,LDataContainer);
                  ViewDialog.grdMonthlySupplyData.Visible         := True;
                  ViewDialog.grdSupplyCompliance.Visible          := False;
                  ViewDialog.grdSupplyComplianceAggregate.Visible := False;
                  ViewDialog.grdIFRStatsComplianceData.Visible    := False;
                end;
              end
              else
              begin
              }
                if (TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetComplianceGridBlockData(
                    LDataContainer,LSupplyDeficitData,FLoadCase,FSequence,FIdentifier,LErrors)) then
                begin
                  PopulateMonthlyDeficitGridData(LDataContainer,LSupplyDeficitData);
                  ViewDialog.grdMonthlySupplyData.Visible         := True;
                  ViewDialog.grdSupplyCompliance.Visible          := False;
                  ViewDialog.grdSupplyComplianceAggregate.Visible := False;
                  ViewDialog.grdIFRStatsComplianceData.Visible    := False;
                end
                else
                  ViewDialog.ShowError(LErrors);
              //end;
            end;
          end;
          btChannelSuppAndDeficitPerc:
          begin
            if (TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(LSupplyDeficitData,btMonthlyAverageChannelFlow,FIdentifier,LErrors)) then
            begin
              LSupplyDeficitData.Clear;
              LSupplyDeficitData.LoadFromFile('R:\WRMF\Deployment\Logs\SupplyDeficit.csv');
              PopulateSupplyDeficitGridData(LSupplyDeficitData);
              ViewDialog.grdMonthlySupplyData.Visible         := False;
              ViewDialog.grdSupplyCompliance.Visible          := True;
              ViewDialog.grdSupplyComplianceAggregate.Visible := False;
            end
            else
              ViewDialog.ShowError(LErrors);
          end;
          btChannelSuppAndCompliencePerc:

          begin
            if (TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(LSupplyDeficitAggregateData,btMonthlyAverageChannelFlow,FIdentifier,LErrors)) then
            begin
              LSupplyDeficitAggregateData.Clear;
              LSupplyDeficitAggregateData.LoadFromFile('R:\WRMF\Deployment\Logs\SupplyDeficitAggregate.csv');
              PopulateSupplyDeficitAggregateGridData(LSupplyDeficitAggregateData);
              ViewDialog.grdMonthlySupplyData.Visible         := False;
              ViewDialog.grdSupplyCompliance.Visible          := False;
              ViewDialog.grdSupplyComplianceAggregate.Visible := True;
            end
            else
              ViewDialog.ShowError(LErrors);
          end;
          btIFRData :
          begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
              LDataContainer,btMonthlyAverageChannelFlow,FIdentifier,LErrors)) then
            begin

              LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                             CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
              if (LIFRFeature <> nil) then
                PopulateIFRData(LIFRFeature,LDataContainer)
              else
              begin
                LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
                if LChannel <> nil then
                  PopulateDemandSupplyChannels(LChannel,LDataContainer);
              end;

            end;
          end;
          btIFRStats :
          begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
              LDataContainer,btMonthlyAverageChannelFlow,FIdentifier,LErrors)) then
            begin
              LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                             CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByNumber(FIdentifier);
              if (LIFRFeature <> nil) then
                PopulateIFRStats(LIFRFeature,LDataContainer)
              else
              begin
                LChannel := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
                if LChannel <> nil then
                  PopulateDemandSupplyChannels(LChannel,LDataContainer);
              end;
            end;
          end;
        end;
        if (FUnits = ouMcmPerMonthOrYear) then
        begin
          if ViewDialog.grdMonthlySupplyData.Visible then
            CalculateMCMPerMonthGridValues(ViewDialog.grdMonthlySupplyData);
          if ViewDialog.grdSupplyCompliance.Visible then
            CalculateMCMPerMonthGridValues(ViewDialog.grdSupplyCompliance);
          if ViewDialog.grdIFRStatsComplianceData.Visible then
            CalculateMCMPerMonthGridValues(ViewDialog.grdIFRStatsComplianceData);
          if ViewDialog.grdSupplyComplianceAggregate.Visible then
            CalculateMCMPerMonthGridValues(ViewDialog.grdSupplyComplianceAggregate);
        end;
      finally
        LDataContainer.Free;
        LSupplyDeficitData.Free;
        LSupplyDeficitAggregateData.Free;
      end;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.SetCurrentViewData;
const OPNAME = 'TOutputComplianceGridValidator.SetCurrentViewData';
var
  LIndex: integer;
begin
  try
{    LIndex := Integer(ViewDialog.cmbViewDataType.Items.Objects[ViewDialog.cmbViewDataType.ItemIndex]);
    if(LIndex < 0) then
      LIndex := 0;
    FCurrentViewData := TOutputDataType(LIndex);  }

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

procedure TOutputComplianceGridValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputComplianceGridValidator.OnViewDataTypeChange';
begin
  try
    SetCurrentViewData;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.PopulateMonthlyDeficitGridData(AData,ADeficts: TStrings);
const OPNAME = 'TOutputComplianceGridValidator.PopulateMonthlyDeficitGridData';
var
  LIndex,
  LCount: integer;
  LMonthlyValues: TStringList;
  LMonthlyValue : double;
  LAverages : array[0..13] of double;
  LFieldProperty    : TAbstractFieldProperty;
begin
  try
    LockWindowUpdate(FPanel.Handle);
    try
      ClearGrid;
      ViewDialog.grdMonthlySupplyData.CellColor := clRed;
      ViewDialog.grdMonthlySupplyData.RowCount := Max(2,AData.Count + 3);
      if(AData.Count > 0) then
      begin
        ViewDialog.grdMonthlySupplyData.Cells[0,1] := FAppModules.Language.GetString('MonthGrid.Year');
        ViewDialog.grdMonthlySupplyData.Cells[13,1] := FAppModules.Language.GetString('MonthGrid.Average');
        for LIndex := 1 to 12 do
        begin
          ViewDialog.grdMonthlySupplyData.Cells[LIndex,1] :=
          TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LIndex];
        end;
        for LIndex := 1 to 2 do
            ADeficts.Insert(0,'N,N,N,N,N,N,N,N,N,N,N,N,N,N');
        ViewDialog.grdMonthlySupplyData.Coloring.Assign(ADeficts);
        //.LoadFromFile('R:\WRMF\Deployment\Logs\Deficits.csv');

        for LIndex := Low(LAverages) to High(LAverages) do
          LAverages[LIndex] := 0.0;

        LMonthlyValues := TStringList.Create;
        try
          for LIndex := 0 to AData.Count -1 do
          begin
            LMonthlyValues.CommaText := AData.Strings[LIndex];
            for LCount := 0 to LMonthlyValues.Count -1 do
            begin
              LMonthlyValue := StrToFloatDef(LMonthlyValues.Strings[LCount],0);

              if LCount >= 1 then
              begin
                if (FUnits = ouMcmPerMonthOrYear) then
                begin
                  LMonthlyValue := (LMonthlyValue * 86400)/1000000;
                  LMonthlyValues.Strings[LCount] :=FormatFloat('######0.000',StrToFloatDef(LMonthlyValues.Strings[LCount],0.000)* 86400/1000000);
                end
                else
                if (FUnits = ouMegaLitersPerDay) then
                begin
                  LMonthlyValue  := (LMonthlyValue * 86400)/1000;
                  LMonthlyValues.Strings[LCount] :=FormatFloat('######0.000',StrToFloatDef(LMonthlyValues.Strings[LCount],0.000)* 86400/1000);
                end;
              end;
              LAverages[LCount] := LAverages[LCount] + LMonthlyValue;
              ViewDialog.grdMonthlySupplyData.Cells[LCount,LIndex+2] := LMonthlyValues.Strings[LCount];
            end;
          end;
          LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
          for LIndex := Low(LAverages)+1 to High(LAverages) do
          begin
            LAverages[LIndex] := LAverages[LIndex] / AData.Count;
            ViewDialog.grdMonthlySupplyData.Cells[LIndex,ViewDialog.grdMonthlySupplyData.RowCount-1] :=
              Format(LFieldProperty.FormatStringGrid {'######0.000'},[LAverages[LIndex]]);
          end;
          ViewDialog.grdMonthlySupplyData.Cells[0,ViewDialog.grdMonthlySupplyData.RowCount-1] := FAppModules.Language.GetString('MonthGrid.Average');
        finally
          LMonthlyValues.Free;
        end;
      end;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.PopulateSupplyDeficitGridData(AData: TStrings);
const OPNAME = 'TOutputComplianceGridValidator.PopulateSupplyDeficitGridData';
var
  LIndex,
  LCount: integer;
  LTotalPercentage : double;
  LColumnValues    : TStringList;
  LFieldProperty   : TAbstractFieldProperty;
begin
  try
    LockWindowUpdate(FPanel.Handle);
    try
      ClearGrid;
      ViewDialog.grdSupplyCompliance.GridType  := gtNormal;
      ViewDialog.grdSupplyCompliance.CellColor := clRed;
      ViewDialog.grdSupplyCompliance.RowCount := Max(2,AData.Count + 2);
      if(AData.Count > 0) then
      begin
        ViewDialog.grdSupplyCompliance.Cells[0,0]  := FAppModules.Language.GetString('MonthGrid.ChannelNo');
        ViewDialog.grdSupplyCompliance.Cells[1,0]  := FAppModules.Language.GetString('MonthGrid.Comply');
        ViewDialog.grdSupplyCompliance.Cells[2,0]  := FAppModules.Language.GetString('MonthGrid.Category');
        ViewDialog.grdSupplyCompliance.Cells[3,0]  := FAppModules.Language.GetString('MonthGrid.SupplyVolume');
        ViewDialog.grdSupplyCompliance.Cells[8,0]  := FAppModules.Language.GetString('MonthGrid.DeficitVolume');

        ViewDialog.grdSupplyCompliance.Cells[3,1]  := '1:20';
        ViewDialog.grdSupplyCompliance.Cells[4,1]  := '1:50';
        ViewDialog.grdSupplyCompliance.Cells[5,1]  := '1:100';
        ViewDialog.grdSupplyCompliance.Cells[6,1]  := '1:200';
        ViewDialog.grdSupplyCompliance.Cells[7,1]  := FAppModules.Language.GetString('MonthGrid.Total');
        ViewDialog.grdSupplyCompliance.Cells[8,1]  := '1:20';
        ViewDialog.grdSupplyCompliance.Cells[9,1]  := '1:50';
        ViewDialog.grdSupplyCompliance.Cells[10,1] := '1:100';
        ViewDialog.grdSupplyCompliance.Cells[11,1] := '1:200';
        ViewDialog.grdSupplyCompliance.Cells[12,1] := FAppModules.Language.GetString('MonthGrid.Total');

        LColumnValues    := TStringList.Create;
        LTotalPercentage := 0.0;
        LFieldProperty    := FAppModules.FieldProperties.FieldProperty('TotalPercentage');
        ViewDialog.grdSupplyCompliance.Coloring.LoadFromFile('R:\WRMF\Deployment\Logs\Compliance.csv');
        try
          for LIndex := 0 to AData.Count -1 do
          begin
            LColumnValues.CommaText := AData[Lindex];
            for LCount := 0 to LColumnValues.Count do
            begin
              if (LCount > 7) and
                 (LCount < 12) then
              begin
                LTotalPercentage := LTotalPercentage + StrToFloatDef(LColumnValues[LCount-1],0.0);
                ViewDialog.grdSupplyCompliance.Cells[LCount, LIndex+2] := LColumnValues[LCount-1] + '%';
                if (LCount = 11) then
                begin
                  ViewDialog.grdSupplyCompliance.Cells[LCount+1,LIndex+2] :=
                    FormatFloat(LFieldProperty.FormatStringGrid {'######0.0'},LTotalPercentage) + '%';
                  LTotalPercentage := 0.0;
                end;
              end
              else
              if (LCount > 2) and
                 (LCount < 7) then
              begin
                LTotalPercentage := LTotalPercentage + StrToFloatDef(LColumnValues[LCount],0.0);
                ViewDialog.grdSupplyCompliance.Cells[LCount,LIndex+2] := LColumnValues[LCount] + '%';
              end
              else
              if (LCount = 7) then
              begin
                ViewDialog.grdSupplyCompliance.Cells[LCount,LIndex+2] :=
                  FormatFloat(LFieldProperty.FormatStringGrid {'######0.0'},LTotalPercentage) + '%';
                LTotalPercentage := 0.0;
              end
              else
              begin
                ViewDialog.grdSupplyCompliance.Cells[LCount,LIndex+2] := LColumnValues[LCount];
                LTotalPercentage := 0.0;
              end;
            end;
          end;
        finally
          LColumnValues.Free;
        end;
      end;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.PopulateSupplyDeficitAggregateGridData(AData: TStrings);
const OPNAME = 'TOutputComplianceGridValidator.PopulateSupplyDeficitAggregateGridData';
var
  LIndex,
  LCount: integer;
  LTotalPercentage : double;
  LColumnValues    : TStringList;
  LFieldProperty   : TAbstractFieldProperty;
begin
  try
    LockWindowUpdate(FPanel.Handle);
    try
      ClearGrid;
      ViewDialog.grdSupplyComplianceAggregate.CellColor := clRed;
      ViewDialog.grdSupplyComplianceAggregate.RowCount := Max(4,AData.Count + 4);
      if(AData.Count > 0) then
      begin
        ViewDialog.grdSupplyComplianceAggregate.Cells[0,0]  := FAppModules.Language.GetString('MonthGrid.AreaNo');
        ViewDialog.grdSupplyComplianceAggregate.Cells[1,0]  := FAppModules.Language.GetString('MonthGrid.ComplianceChecks');
        ViewDialog.grdSupplyComplianceAggregate.Cells[3,0]  := FAppModules.Language.GetString('MonthGrid.SupplyVolume');
        ViewDialog.grdSupplyComplianceAggregate.Cells[8,0]  := FAppModules.Language.GetString('MonthGrid.DeficitVolume');

        ViewDialog.grdSupplyComplianceAggregate.Cells[1,1]  := FAppModules.Language.GetString('MonthGrid.TotalArea');
        ViewDialog.grdSupplyComplianceAggregate.Cells[2,1]  := FAppModules.Language.GetString('MonthGrid.IndividualChannel');

        ViewDialog.grdSupplyComplianceAggregate.Cells[3,1]  := '1:20';
        ViewDialog.grdSupplyComplianceAggregate.Cells[4,1]  := '1:50';
        ViewDialog.grdSupplyComplianceAggregate.Cells[5,1]  := '1:100';
        ViewDialog.grdSupplyComplianceAggregate.Cells[6,1]  := '1:200';
        ViewDialog.grdSupplyComplianceAggregate.Cells[7,1]  := 'Total';
        ViewDialog.grdSupplyComplianceAggregate.Cells[8,1]  := '1:20';
        ViewDialog.grdSupplyComplianceAggregate.Cells[9,1]  := '1:50';
        ViewDialog.grdSupplyComplianceAggregate.Cells[10,1] := '1:100';
        ViewDialog.grdSupplyComplianceAggregate.Cells[11,1] := '1:200';
        ViewDialog.grdSupplyComplianceAggregate.Cells[12,1] := 'Total';

        ViewDialog.grdSupplyComplianceAggregate.Coloring.LoadFromFile('R:\WRMF\Deployment\Logs\Aggregate.csv');

        LColumnValues    := TStringList.Create;
        LTotalPercentage := 0.0;
        LFieldProperty    := FAppModules.FieldProperties.FieldProperty('TotalPercentage');
        try
          for LIndex := 0 to AData.Count -1 do
          begin
            LColumnValues.CommaText := AData[Lindex];
            for LCount := 0 to LColumnValues.Count do
            begin
              if (LCount > 7) and
                 (LCount < 12) then
              begin
                LTotalPercentage := LTotalPercentage + StrToFloatDef(LColumnValues[LCount-1],0.0);
                ViewDialog.grdSupplyComplianceAggregate.Cells[LCount, LIndex+3] := LColumnValues[LCount-1] + '%';
                if (LCount = 11) then
                begin
                  ViewDialog.grdSupplyComplianceAggregate.Cells[LCount+1,LIndex+3] :=
                    FormatFloat(LFieldProperty.FormatStringGrid {'######0.0'},LTotalPercentage) + '%';
                  LTotalPercentage := 0.0;
                end;
              end
              else
              if (LCount > 2) and
                 (LCount < 7) then
              begin
                LTotalPercentage := LTotalPercentage + StrToFloatDef(LColumnValues[LCount],0.0);
                ViewDialog.grdSupplyComplianceAggregate.Cells[LCount,LIndex+3] := LColumnValues[LCount] + '%';
              end
              else
              if (LCount = 7) then
              begin
                ViewDialog.grdSupplyComplianceAggregate.Cells[LCount,LIndex+3] :=
                  FormatFloat(LFieldProperty.FormatStringGrid {'######0.0'},LTotalPercentage) + '%';
                LTotalPercentage := 0.0;
              end
              else
              begin
                ViewDialog.grdSupplyComplianceAggregate.Cells[LCount,LIndex+3] := LColumnValues[LCount];
                LTotalPercentage := 0.0;
              end;
            end;
          end;
        finally
          LColumnValues.Free;
        end;
      end;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.GetMonthData(AData: TStrings;AMonth : integer);
const OPNAME = 'TOutputComplianceGridValidator.GetMonthData';
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

function TOutputComplianceGridValidator.GetSupplyValueByYear(ASupplyYear : integer;AMonth : integer;AData: TStrings) : double;
const OPNAME = 'TOutputComplianceGridValidator.GetSupplyValueByYear';
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

procedure TOutputComplianceGridValidator.PopulateIFRStats(AIFRFeature : TIFRFeature;AData : TStrings);
const OPNAME = 'TOutputComplianceGridValidator.PopulateIFRStats';
var
  LIndex : integer;
  LReferenceUnrankedData,
  LReferenceData : TStringList;
  LRequirement : double;
  LSupply : double;
  LDifference : double;
  LPercentageOfFailure : double;
  LCount : integer;
  LRequiredAverage : array[1..12] of double;
  LSupplyAverage : array[1..12] of double;
  LDifferenceAverage : array[1..12] of double;
  LNonZeroDifferenceCount : array[1..12] of integer;
  LAvgPercentageOfFailure : array[1..12] of double;
begin
  try
    ClearGrid;
    ViewDialog.grdIFRStatsComplianceData.Visible := True;
    ViewDialog.grdIFRStatsComplianceData.ColCount := 7;
    ViewDialog.grdIFRStatsComplianceData.RowCount := 14;
    ViewDialog.grdIFRStatsComplianceData.Heading := FAppModules.Language.GetString('TIFRChannelComplianceGrid.IFRChannelHeading');
    ViewDialog.grdIFRStatsComplianceData.Cells[0,1] := FAppModules.Language.GetString('NetworkFeatures.Month');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[0] := 80;
    for LIndex := 1 to 12 do
    begin
      ViewDialog.grdIFRStatsComplianceData.Cells[0,LIndex+1] :=
      TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNameByIndex[LIndex];
    end;

    ViewDialog.grdIFRStatsComplianceData.Cells[1,1] :=
                                       FAppModules.Language.GetString('TIFRChannelComplianceGrid.IFRChannelCount');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[1] := 100;

    ViewDialog.grdIFRStatsComplianceData.Cells[2,1] :=
                                       FAppModules.Language.GetString('TIFRChannelComplianceGrid.IFRChannelFailure');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[2] := 100;

    ViewDialog.grdIFRStatsComplianceData.Cells[3,1] := '%';
    ViewDialog.grdIFRStatsComplianceData.ColWidths[3] := 60;

    ViewDialog.grdIFRStatsComplianceData.Cells[4,1] := FAppModules.Language.GetString(
                                                       'TIFRChannelComplianceGrid.RequiredIFR');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[4] := 100;

    ViewDialog.grdIFRStatsComplianceData.Cells[5,1] := FAppModules.Language.GetString(
                                                       'TIFRChannelComplianceGrid.SuppliedIFR');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[5] := 100;

    ViewDialog.grdIFRStatsComplianceData.Cells[6,1] := FAppModules.Language.GetString(
                                                       'TIFRChannelComplianceGrid.RequiredAndSuppliedDifference');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[6] := 100;

    if (AIFRFeature <> nil) and (AData <> nil ) then
    begin
      if (AData.Count > 0) then
      begin
        LReferenceData := TStringList.Create;
        LReferenceUnrankedData := TStringList.Create;
        try
//          LReferenceUnrankedData.CommaText := AData.CommaText;
          if not AIFRFeature.GetNodeReferenceFlowData(LReferenceUnrankedData) then
            Exit;
          for LIndex := 1 to 12 do
          begin
            LReferenceData.CommaText := LReferenceUnrankedData.CommaText;
            GetMonthData(LReferenceData,LIndex);
            if AData.Count <> LReferenceData.Count then
              Exit;
            for LCount := 0 to LReferenceData.Count-1 do
            begin
              LReferenceData[LCount];
              LRequirement := StrToFloat(FormatFloat('00000000000000.000',
                       AIFRFeature.GetRequirementFlowFromReferenceFlow(LIndex,StrToFloat(LReferenceData[LCount]))));
              LSupply := StrToFloat(FormatFloat('00000000000000.000',
                       GetSupplyValueByYear(integer(LReferenceData.Objects[LCount]), LIndex,AData)));
              LDifference := 0;
              if LRequirement >= LSupply then
                LDifference := LRequirement - LSupply;
              LPercentageOfFailure := 0;
              if LRequirement > 0 then
                LPercentageOfFailure :=  100-((LSupply/LRequirement)*100);
              LRequiredAverage[LIndex] := LRequiredAverage[LIndex] + LRequirement;
              LSupplyAverage[LIndex] := LSupplyAverage[LIndex] + LSupply;
              LAvgPercentageOfFailure[LIndex] := LAvgPercentageOfFailure[LIndex] + LPercentageOfFailure;
              if LDifference > 0 then
              begin
                LDifferenceAverage[LIndex] := LDifferenceAverage[LIndex] + LDifference;
                LNonZeroDifferenceCount[LIndex] := LNonZeroDifferenceCount[LIndex] + 1;
              end;
            end;
            ViewDialog.grdIFRStatsComplianceData.Cells[1,LIndex+1] := Format('%d', [LReferenceData.Count]);
            ViewDialog.grdIFRStatsComplianceData.Cells[2,LIndex+1] := Format('%d', [LNonZeroDifferenceCount[LIndex]]);
            ViewDialog.grdIFRStatsComplianceData.Cells[2,LIndex+1] := Format('%d', [LNonZeroDifferenceCount[LIndex]]);
            ViewDialog.grdIFRStatsComplianceData.Cells[3,LIndex+1] := Format('%3f', [(LNonZeroDifferenceCount[LIndex]/LReferenceData.Count)*100])+'%';
            ViewDialog.grdIFRStatsComplianceData.Cells[4,LIndex+1] := Format('%6.3f', [LRequiredAverage[LIndex]/LReferenceData.Count]);
            ViewDialog.grdIFRStatsComplianceData.Cells[5,LIndex+1] := Format('%6.3f', [LSupplyAverage[LIndex]/LReferenceData.Count]);
            if (LRequiredAverage[LIndex]/LReferenceData.Count)-(LSupplyAverage[LIndex]/LReferenceData.Count) > 0 then
              ViewDialog.grdIFRStatsComplianceData.Cells[6,LIndex+1] := Format('%6.3f', [(LRequiredAverage[LIndex]/LReferenceData.Count)-(LSupplyAverage[LIndex]/LReferenceData.Count)])
            else
              ViewDialog.grdIFRStatsComplianceData.Cells[6,LIndex+1] := '0.000';
          end;
        finally
          LReferenceData.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGridValidator.PopulateIFRData(AIFRFeature : TIFRFeature;AData : TStrings);
const OPNAME = 'TOutputComplianceGridValidator.PopulateIFRData';
var
  LIndex : integer;
  LReferenceUnRankedData,
  LReferenceData : TStringList;
  LRequirement : double;
  LSupply : double;
  LDifference : double;
  LPercentageOfFailure : double;
  LMonth,
  LCount : integer;
  LRequiredAverage : array[1..12] of double;
  LSupplyAverage : array[1..12] of double;
  LDifferenceAverage : array[1..12] of double;
  LAnnualRequiredAverage : array of double;
  LAnnualSupplyAverage : array of double;
  LAnnualDifferenceAverage : array of double;
  LGrandAnnualRequiredAverage : double;
  LGrandAnnualSupplyAverage : double;
  LDaysInMonth : double;
begin
  try
    ClearGrid;
    ViewDialog.grdIFRStatsComplianceData.Visible := True;
    ViewDialog.grdIFRStatsComplianceData.ColCount := 51;
    ViewDialog.grdIFRStatsComplianceData.Cells[0,1] := FAppModules.Language.GetString('OutputReview.OutputGraphYear');
    LMonth := 2;
    for LIndex := 0 to 11 do
    begin
      ViewDialog.grdIFRStatsComplianceData.Cells[LMonth,0] :=
      TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNameByIndex[LIndex+1];
      LMonth := LMonth + 4;
    end;
    LCount := 1;
    for LIndex := 1 to 12 do
    begin
      ViewDialog.grdIFRStatsComplianceData.Cells[LCount,1] := FAppModules.Language.GetString(
                                                             'TIFRChannelComplianceGrid.RequiredIFR');
      ViewDialog.grdIFRStatsComplianceData.ColWidths[LCount] := 150;
      ViewDialog.grdIFRStatsComplianceData.Cells[LCount+1,1] := FAppModules.Language.GetString(
                                                                'TIFRChannelComplianceGrid.SuppliedIFR');
      ViewDialog.grdIFRStatsComplianceData.ColWidths[LCount+1] := 150;
      ViewDialog.grdIFRStatsComplianceData.Cells[LCount+2,1] := FAppModules.Language.GetString(
                                                        'TIFRChannelComplianceGrid.RequiredAndSuppliedDifference');
      ViewDialog.grdIFRStatsComplianceData.ColWidths[LCount+2] := 150;
      ViewDialog.grdIFRStatsComplianceData.Cells[LCount+3,1] := FAppModules.Language.GetString(
                                                        'TIFRChannelComplianceGrid.PercentageOfFailure');
      ViewDialog.grdIFRStatsComplianceData.ColWidths[LCount+3] := 150;
      LCount := LCount + 4;
    end;
    ViewDialog.grdIFRStatsComplianceData.Cells[49,1] := FAppModules.Language.GetString(
                                                        'TIFRChannelComplianceGrid.AverageRequired');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[49] := 150;
    ViewDialog.grdIFRStatsComplianceData.Cells[50,1] := FAppModules.Language.GetString(
                                                        'TIFRChannelComplianceGrid.AverageSupplied');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[50] := 150;
    if (AIFRFeature <> nil) and (AData <> nil ) then
    begin
      if (AData.Count > 0) then
      begin
        ViewDialog.grdIFRStatsComplianceData.RowCount := AData.Count+3;
        SetLength(LAnnualRequiredAverage,Adata.Count);
        SetLength(LAnnualSupplyAverage,Adata.Count);
        SetLength(LAnnualDifferenceAverage,Adata.Count);
        LReferenceData := TStringList.Create;
        LReferenceUnRankedData := TStringList.Create;
        try
          for LIndex := 1 to 12 do
          begin
            LRequiredAverage[LIndex] := 0;
            LSupplyAverage[LIndex] := 0;
            LDifferenceAverage[LIndex] := 0;
          end;
          LMonth := 1;
          if not AIFRFeature.GetNodeReferenceFlowData(LReferenceUnRankedData) then
            Exit;

          for LIndex := 1 to 12 do
          begin
            LReferenceData.CommaText := LReferenceUnRankedData.CommaText;
            GetMonthData(LReferenceData,LIndex);
            LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).
                            CastRunConfigurationData.MonthDaysByIndex[LIndex];
            if AData.Count <> LReferenceData.Count then
              Exit;
            for LCount := 0 to LReferenceData.Count-1 do
            begin
              LRequirement := StrToFloat(FormatFloat('00000000000000.000',
                       AIFRFeature.GetRequirementFlowFromReferenceFlow(LIndex,StrToFloat(LReferenceData[LCount]))));
              LSupply := StrToFloat(FormatFloat('00000000000000.000',
                       GetSupplyValueByYear(integer(LReferenceData.Objects[LCount]), LIndex,AData)));
              LDifference := 0;
              LPercentageOfFailure := 0;
              if LRequirement >= LSupply then
                LDifference := LRequirement - LSupply;
              if LRequirement > 0 then
                LPercentageOfFailure :=  100-((LSupply/LRequirement)*100);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[0] := 60;
              ViewDialog.grdIFRStatsComplianceData.Cells[0,LCount+2] := IntToStr(integer(LReferenceData.Objects[LCount]));
              ViewDialog.grdIFRStatsComplianceData.Cells[LMonth,LCount+2] := Format('%6.3f', [LRequirement]);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[LMonth] := 100;
              ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+1,LCount+2] := Format('%6.3f', [LSupply]);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[LMonth+1] := 100;
              ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+2,LCount+2] := Format('%6.3f', [LDifference]);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[LMonth+2] := 100;
              ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+3,LCount+2] := Format('%6.3f', [LPercentageOfFailure]);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[LMonth+3] := 100;

              LRequiredAverage[LIndex] := LRequiredAverage[LIndex] + LRequirement;
              LSupplyAverage[LIndex] := LSupplyAverage[LIndex] + LSupply;
              LDifferenceAverage[LIndex] := LDifferenceAverage[LIndex] + LDifference;

              LAnnualRequiredAverage[LCount] := LAnnualRequiredAverage[LCount] + ((LDaysInMonth*LRequirement));
              LAnnualSupplyAverage[LCount] := LAnnualSupplyAverage[LCount] + ((LDaysInMonth*LSupply));
              LAnnualDifferenceAverage[LCount] := LDifferenceAverage[LCount] + ((LDaysInMonth*LDifference));
            end;
            ViewDialog.grdIFRStatsComplianceData.Cells[0,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
                        FAppModules.Language.GetString('TIFRChannelComplianceGrid.MonthlyAverage');
            ViewDialog.grdIFRStatsComplianceData.Cells[LMonth,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
            Format('%6.3f', [LRequiredAverage[LIndex]/LReferenceData.Count]);
            ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+1,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
            Format('%6.3f', [LSupplyAverage[LIndex]/LReferenceData.Count]);
            ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+2,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
            Format('%6.3f', [LDifferenceAverage[LIndex]/LReferenceData.Count]);
            ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+3,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
            Format('%6.3f', [LDifferenceAverage[LIndex]/LReferenceData.Count]);
            LMonth := LMonth + 4;
          end;
          LGrandAnnualRequiredAverage := 0;
          LGrandAnnualSupplyAverage   := 0;
          for LCount := 0 to LReferenceData.Count-1 do
          begin
            LGrandAnnualRequiredAverage := LGrandAnnualRequiredAverage + LAnnualRequiredAverage[LCount]/364.25;
            LGrandAnnualSupplyAverage := LGrandAnnualSupplyAverage + LAnnualSupplyAverage[LCount]/364.25;
            ViewDialog.grdIFRStatsComplianceData.Cells[49,LCount+2] := Format('%6.3f', [LAnnualRequiredAverage[LCount]/364.25]);
            ViewDialog.grdIFRStatsComplianceData.Cells[50,LCount+2] := Format('%6.3f', [LAnnualSupplyAverage[LCount]/364.25]);
          end;
          ViewDialog.grdIFRStatsComplianceData.Cells[49,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
          Format('%6.3f', [LGrandAnnualRequiredAverage/LReferenceData.Count]);
          ViewDialog.grdIFRStatsComplianceData.Cells[50,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
          Format('%6.3f', [LGrandAnnualSupplyAverage/LReferenceData.Count]);

        finally
          LReferenceData.Free;
          LReferenceUnRankedData.Free;
          Finalize(LAnnualRequiredAverage);
          Finalize(LAnnualSupplyAverage);
          Finalize(LAnnualDifferenceAverage);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGridValidator.PopulateDemandSupplyChannels(AChannel : IGeneralFlowChannel;AData: TStrings);
const OPNAME = 'TOutputComplianceGridValidator.PopulateDemandSupplyChannels';
begin
  try
    if (AChannel <> nil) then
    begin
      if(AChannel.ChannelType = 2) then
        PopulateDemandSupplyValues(AData,AChannel)
      else if(AChannel.ChannelType = 8) and (AChannel.IFRFeature = nil) then
        PopulateDemandSupplyValues(AData,AChannel)
      else if(AChannel.ChannelType = 11) then
        PopulateDemandSupplyValues(AData,AChannel)
      else if(AChannel.ChannelType = 14) then
        PopulateDemandSupplyValues(AData,AChannel);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGridValidator.PopulateDemandSupplyValues(AData: TStrings;AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputComplianceGridValidator.PopulateDemandSupplyChannels';
var
  LChannelDemandValues : TStringList;
begin
  try
    LChannelDemandValues := TStringList.Create;
    try
      PopulateChannelDemandData(LChannelDemandValues,AChannel);
      if LChannelDemandValues.Count>0 then
      begin
        case FCurrentViewData of
          btIFRData : PopulateData(LChannelDemandValues,AData);
          btIFRStats : PopulateStats(LChannelDemandValues,AData);
        end;
      end;
    finally
      FreeAndNil(LChannelDemandValues);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGridValidator.PopulateData(AChannelDemandValues,AData: TStrings);
const OPNAME = 'TOutputComplianceGridValidator.PopulateData';
var
  LIndex : integer;
  LDemandLineData : TStringList;
  LSupplyLineData : TStringList;
  LDaysInMonth,
  LRequirement : double;
  LSupply : double;
  LDifference : double;
  LPercentageOfFailure : double;
  LMonth,
  LCount : integer;
  LRequiredAverage : array[1..12] of double;
  LSupplyAverage : array[1..12] of double;
  LDifferenceAverage : array[1..12] of double;
  LAnnualRequiredAverage : array of double;
  LAnnualSupplyAverage : array of double;
  LAnnualDifferenceAverage : array of double;
  LGrandAnnualRequiredAverage : double;
  LGrandAnnualSupplyAverage : double;
begin
  try
    ClearGrid;
    ViewDialog.grdIFRStatsComplianceData.Visible := True;
    ViewDialog.grdIFRStatsComplianceData.ColCount := 51;
    ViewDialog.grdIFRStatsComplianceData.Cells[0,1] := FAppModules.Language.GetString('OutputReview.OutputGraphYear');
    LMonth := 2;
    for LIndex := 0 to 11 do
    begin
      ViewDialog.grdIFRStatsComplianceData.Cells[LMonth,0] :=
      TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNameByIndex[LIndex+1];
      LMonth := LMonth + 4;
    end;
    LCount := 1;
    for LIndex := 1 to 12 do
    begin
      ViewDialog.grdIFRStatsComplianceData.Cells[LCount,1] := 'Demand';
      //FAppModules.Language.GetString('TIFRChannelComplianceGrid.RequiredIFR');
      ViewDialog.grdIFRStatsComplianceData.ColWidths[LCount] := 150;
      ViewDialog.grdIFRStatsComplianceData.Cells[LCount+1,1] := 'Supply';
      //FAppModules.Language.GetString('TIFRChannelComplianceGrid.SuppliedIFR');
      ViewDialog.grdIFRStatsComplianceData.ColWidths[LCount+1] := 150;
      ViewDialog.grdIFRStatsComplianceData.Cells[LCount+2,1] := 'Difference';
      //FAppModules.Language.GetString('TIFRChannelComplianceGrid.RequiredAndSuppliedDifference');
      ViewDialog.grdIFRStatsComplianceData.ColWidths[LCount+2] := 150;
      ViewDialog.grdIFRStatsComplianceData.Cells[LCount+3,1] := '% Of Failure';
      //FAppModules.Language.GetString('TIFRChannelComplianceGrid.PercentageOfFailure');
      ViewDialog.grdIFRStatsComplianceData.ColWidths[LCount+3] := 150;
      LCount := LCount + 4;
    end;
    ViewDialog.grdIFRStatsComplianceData.Cells[49,1] := 'Average Demand';
    //FAppModules.Language.GetString('TIFRChannelComplianceGrid.AverageRequired');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[49] := 150;
    ViewDialog.grdIFRStatsComplianceData.Cells[50,1] := 'Average Supply';
    //FAppModules.Language.GetString('TIFRChannelComplianceGrid.AverageSupplied');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[50] := 150;
    if (AChannelDemandValues <> nil) and (AData <> nil ) then
    begin
      if (AData.Count > 0) then
      begin
        ViewDialog.grdIFRStatsComplianceData.RowCount := AData.Count+3;
        LDemandLineData := TStringList.Create;
        LSupplyLineData := TStringList.Create;
        SetLength(LAnnualRequiredAverage,Adata.Count);
        SetLength(LAnnualSupplyAverage,Adata.Count);
        SetLength(LAnnualDifferenceAverage,Adata.Count);

        try
          for LIndex := 1 to 12 do
          begin
            LRequiredAverage[LIndex] := 0;
            LSupplyAverage[LIndex] := 0;
            LDifferenceAverage[LIndex] := 0;
          end;
          LMonth := 1;
          for LIndex := 1 to 12 do
          begin
            LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).
                            CastRunConfigurationData.MonthDaysByIndex[LIndex];
            for LCount := 0 to AChannelDemandValues.Count-1 do
            begin
              if LCount<AData.Count then
                LSupplyLineData.CommaText := AData[LCount]
              else
                Break;

              LDemandLineData.CommaText := AChannelDemandValues[LCount];
              LRequirement := 0;
              LSupply := 0;
              if LDemandLineData.Count>LIndex then
                LRequirement := StrToFloat(FormatFloat('00000000000000.000',StrToFloat(LDemandLineData[LIndex])));
              if LSupplyLineData.Count>LIndex then
                LSupply := StrToFloat(FormatFloat('00000000000000.000',StrToFloat(LSupplyLineData[LIndex])));
              LDifference := 0;
              LPercentageOfFailure := 0;
              if LRequirement >= LSupply then
                LDifference := LRequirement - LSupply;
              if LRequirement > 0 then
                LPercentageOfFailure :=  100-((LSupply/LRequirement)*100);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[0] := 60;
              ViewDialog.grdIFRStatsComplianceData.Cells[0,LCount+2] := LDemandLineData[0];
              ViewDialog.grdIFRStatsComplianceData.Cells[LMonth,LCount+2] := Format('%6.3f', [LRequirement]);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[LMonth] := 100;
              ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+1,LCount+2] := Format('%6.3f', [LSupply]);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[LMonth+1] := 100;
              ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+2,LCount+2] := Format('%6.3f', [LDifference]);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[LMonth+2] := 100;
              ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+3,LCount+2] := Format('%6.3f', [LPercentageOfFailure]);
              ViewDialog.grdIFRStatsComplianceData.ColWidths[LMonth+3] := 100;

              LRequiredAverage[LIndex] := LRequiredAverage[LIndex] + LRequirement;
              LSupplyAverage[LIndex] := LSupplyAverage[LIndex] + LSupply;
              LDifferenceAverage[LIndex] := LDifferenceAverage[LIndex] + LDifference;

              LAnnualRequiredAverage[LCount] := LAnnualRequiredAverage[LCount] + ((LDaysInMonth*LRequirement));
              LAnnualSupplyAverage[LCount] := LAnnualSupplyAverage[LCount] + ((LDaysInMonth*LSupply));
              LAnnualDifferenceAverage[LCount] := LDifferenceAverage[LCount] + ((LDaysInMonth*LDifference));


            end;

            ViewDialog.grdIFRStatsComplianceData.Cells[0,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
                        FAppModules.Language.GetString('TIFRChannelComplianceGrid.MonthlyAverage');

            ViewDialog.grdIFRStatsComplianceData.Cells[LMonth,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
            Format('%6.3f', [LRequiredAverage[LIndex]/LDemandLineData.Count]);
            ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+1,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
            Format('%6.3f', [LSupplyAverage[LIndex]/LDemandLineData.Count]);
            ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+2,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
            Format('%6.3f', [LDifferenceAverage[LIndex]/LDemandLineData.Count]);
            ViewDialog.grdIFRStatsComplianceData.Cells[LMonth+3,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
            Format('%6.3f', [LDifferenceAverage[LIndex]/LDemandLineData.Count]);
            LMonth := LMonth + 4;
          end;
          LGrandAnnualRequiredAverage := 0;
          LGrandAnnualSupplyAverage   := 0;
          for LCount := 0 to AChannelDemandValues.Count-1 do
          begin
            LGrandAnnualRequiredAverage := LGrandAnnualRequiredAverage + LAnnualRequiredAverage[LCount]/364.25;
            LGrandAnnualSupplyAverage := LGrandAnnualSupplyAverage + LAnnualSupplyAverage[LCount]/364.25;
            ViewDialog.grdIFRStatsComplianceData.Cells[49,LCount+2] := Format('%6.3f', [LAnnualRequiredAverage[LCount]/364.25]);
            ViewDialog.grdIFRStatsComplianceData.Cells[50,LCount+2] := Format('%6.3f', [LAnnualSupplyAverage[LCount]/364.25]);
          end;
          ViewDialog.grdIFRStatsComplianceData.Cells[49,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
          Format('%6.3f', [LGrandAnnualRequiredAverage/LSupplyLineData.Count]);
          ViewDialog.grdIFRStatsComplianceData.Cells[50,ViewDialog.grdIFRStatsComplianceData.RowCount-1] :=
          Format('%6.3f', [LGrandAnnualSupplyAverage/LSupplyLineData.Count]);
        finally
          LDemandLineData.Free;
          LSupplyLineData.Free;
          Finalize(LAnnualRequiredAverage);
          Finalize(LAnnualSupplyAverage);
          Finalize(LAnnualDifferenceAverage);

        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGridValidator.PopulateStats(AChannelDemandValues,AData: TStrings);
const OPNAME = 'TOutputComplianceGridValidator.PopulateStats';
var
  LIndex : integer;
  LSupplyLineData,
  LDemandLineData : TStringList;
  LRequirement : double;
  LSupply : double;
  LDifference : double;
  LPercentageOfFailure : double;
  LCount : integer;
  LRequiredAverage : array[1..12] of double;
  LSupplyAverage : array[1..12] of double;
  LDifferenceAverage : array[1..12] of double;
  LNonZeroDifferenceCount : array[1..12] of integer;
  LAvgPercentageOfFailure : array[1..12] of double;
begin
  try
    ClearGrid;
    ViewDialog.grdIFRStatsComplianceData.Visible := True;
    ViewDialog.grdIFRStatsComplianceData.ColCount := 7;
    ViewDialog.grdIFRStatsComplianceData.RowCount := 14;
    ViewDialog.grdIFRStatsComplianceData.Heading := FAppModules.Language.GetString('TIFRChannelComplianceGrid.IFRChannelHeading');
    ViewDialog.grdIFRStatsComplianceData.Cells[0,1] := FAppModules.Language.GetString('NetworkFeatures.Month');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[0] := 80;
    for LIndex := 1 to 12 do
    begin
      ViewDialog.grdIFRStatsComplianceData.Cells[0,LIndex+1] :=
      TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNameByIndex[LIndex];
    end;

    ViewDialog.grdIFRStatsComplianceData.Cells[1,1] :=
                                       FAppModules.Language.GetString('TIFRChannelComplianceGrid.IFRChannelCount');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[1] := 100;

    ViewDialog.grdIFRStatsComplianceData.Cells[2,1] := 'Failure';
    //FAppModules.Language.GetString('TIFRChannelComplianceGrid.IFRChannelFailure');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[2] := 100;

    ViewDialog.grdIFRStatsComplianceData.Cells[3,1] := '%';
    ViewDialog.grdIFRStatsComplianceData.ColWidths[3] := 60;

    ViewDialog.grdIFRStatsComplianceData.Cells[4,1] := 'Demand';
    //FAppModules.Language.GetString('TIFRChannelComplianceGrid.RequiredIFR');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[4] := 100;

    ViewDialog.grdIFRStatsComplianceData.Cells[5,1] := 'Supply';
    //FAppModules.Language.GetString('TIFRChannelComplianceGrid.SuppliedIFR');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[5] := 100;

    ViewDialog.grdIFRStatsComplianceData.Cells[6,1] := 'Difference';
    FAppModules.Language.GetString('TIFRChannelComplianceGrid.RequiredAndSuppliedDifference');
    ViewDialog.grdIFRStatsComplianceData.ColWidths[6] := 100;

    if (AChannelDemandValues <> nil) and (AData <> nil ) then
    begin
      if (AData.Count > 0) then
      begin
        LDemandLineData := TStringList.Create;
        LSupplyLineData := TStringList.Create;
        try
          for LIndex := 1 to 12 do
          begin
            for LCount := 0 to AChannelDemandValues.Count-1 do
            begin
              if LCount<AData.Count then
                LSupplyLineData.CommaText := AData[LCount]
              else
                Break;

              LDemandLineData.CommaText := AChannelDemandValues[LCount];
              LRequirement := 0;
              LSupply := 0;
              if LDemandLineData.Count>LIndex then
                LRequirement := StrToFloat(FormatFloat('00000000000000.000',StrToFloat(LDemandLineData[LIndex])));
              if LSupplyLineData.Count>LIndex then
                LSupply := StrToFloat(FormatFloat('00000000000000.000',StrToFloat(LSupplyLineData[LIndex])));
              LDifference := 0;
              LPercentageOfFailure := 0;
              if LRequirement >= LSupply then
                LDifference := LRequirement - LSupply;
              if LDifference > 0 then
                LPercentageOfFailure :=  100-((LSupply/LRequirement)*100);

              LRequiredAverage[LIndex] := LRequiredAverage[LIndex] + LRequirement;
              LSupplyAverage[LIndex] := LSupplyAverage[LIndex] + LSupply;
              LAvgPercentageOfFailure[LIndex] := LAvgPercentageOfFailure[LIndex] + LPercentageOfFailure;
              if LDifference > 0 then
              begin
                LDifferenceAverage[LIndex] := LDifferenceAverage[LIndex] + LDifference;
                LNonZeroDifferenceCount[LIndex] := LNonZeroDifferenceCount[LIndex] + 1;
              end;
            end;
            ViewDialog.grdIFRStatsComplianceData.Cells[1,LIndex+1] := Format('%d', [AChannelDemandValues.Count-1]);
            ViewDialog.grdIFRStatsComplianceData.Cells[2,LIndex+1] := Format('%d', [LNonZeroDifferenceCount[LIndex]]);
            ViewDialog.grdIFRStatsComplianceData.Cells[2,LIndex+1] := Format('%d', [LNonZeroDifferenceCount[LIndex]]);
            ViewDialog.grdIFRStatsComplianceData.Cells[3,LIndex+1] := Format('%3f', [(LNonZeroDifferenceCount[LIndex]/(AChannelDemandValues.Count-1))*100])+'%';
            ViewDialog.grdIFRStatsComplianceData.Cells[4,LIndex+1] := Format('%6.3f', [LRequiredAverage[LIndex]/(AChannelDemandValues.Count-1)]);
            ViewDialog.grdIFRStatsComplianceData.Cells[5,LIndex+1] := Format('%6.3f', [LSupplyAverage[LIndex]/(AChannelDemandValues.Count-1)]);
            //if (LRequiredAverage[LIndex]/AChannelDemandValues.Count-1)-(LSupplyAverage[LIndex]/(AChannelDemandValues.Count-1)) > 0 then
            ViewDialog.grdIFRStatsComplianceData.Cells[6,LIndex+1] := Format('%6.3f', [(LRequiredAverage[LIndex]/(AChannelDemandValues.Count-1))-(LSupplyAverage[LIndex]/(AChannelDemandValues.Count-1))])
            {else
              ViewDialog.grdIFRStatsComplianceData.Cells[6,LIndex+1] := '0.000';
            }
          end;
        finally
          LSupplyLineData.Free;
          LDemandLineData.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGridValidator.ClearGrid;
const OPNAME = 'TOutputComplianceGridValidator.ClearGrid';
var
  LIndex: integer;
  LCol : integer;
begin
  try
    for LIndex := 0 to ViewDialog.grdMonthlySupplyData.ColCount -1 do
      ViewDialog.grdMonthlySupplyData.Cols[LIndex].Clear;
    ViewDialog.grdMonthlySupplyData.ColCount := 14;
    ViewDialog.grdMonthlySupplyData.RowCount := 2;
    for LIndex := 0 to ViewDialog.grdSupplyCompliance.ColCount -1 do
      ViewDialog.grdSupplyCompliance.Cols[LIndex].Clear;
    ViewDialog.grdSupplyCompliance.ColCount := 12;
    ViewDialog.grdSupplyCompliance.RowCount := 2;
    for LIndex := 0 to ViewDialog.grdSupplyComplianceAggregate.ColCount -1 do
      ViewDialog.grdSupplyComplianceAggregate.Cols[LIndex].Clear;
    ViewDialog.grdSupplyComplianceAggregate.ColCount := 13;
    ViewDialog.grdSupplyComplianceAggregate.RowCount := 3;
    for LIndex := 0 to ViewDialog.grdIFRStatsComplianceData.RowCount-1 do
    begin
     for LCol := 0 to ViewDialog.grdIFRStatsComplianceData.ColCount-1 do
       ViewDialog.grdIFRStatsComplianceData.Cells[LCol,LIndex] := '';
    end;
    ViewDialog.grdIFRStatsComplianceData.Visible := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputComplianceGridValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.DoPrint;
const OPNAME = 'TOutputComplianceGridValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.CanExport: boolean;
const OPNAME = 'TOutputComplianceGridValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComplianceGridValidator.CanPrint: boolean;
const OPNAME = 'TOutputComplianceGridValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputComplianceGridValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdComplianceGrid,FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.GetSelectionData;
const OPNAME = 'TOutputComplianceGridValidator.GetSelectionData';
var
  LDataSelection : IOutputDataSelection;
begin
  try
   LDataSelection := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection;
   if (LDataSelection <> nil) then
   begin
     FLoadCase     := LDataSelection.LoadCase;
     FSequence     := LDataSelection.Sequence;
     FMonth        := LDataSelection.Month;
     FUnits        := LDataSelection.Units;
     FValueType    := LDataSelection.ValueType;
     FTimeStep     := LDataSelection.TimeStep;
     FHighLight    := LDataSelection.Highlight;
     FDisplayMonth := LDataSelection.DisplayMonth;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComplianceGridValidator.ChangeViewDataLabel;
const OPNAME = 'TOutputComplianceGridValidator.ChangeViewDataLabel';
begin
  try
    if (FUnits = ouPerSecond) then
      ViewDialog.UnitsLabel.Caption :=  '('+ FAppModules.Language.GetString('MasterControl.M3perSecond')+')'
    else if (FUnits = ouMcmPerMonthOrYear) then
      ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.M3perMonth')
//    else if (FUnits = ouPercentage) then
//      ViewDialog.UnitsLabel.Caption := '%'
    else if (FUnits = ouMegaLitersPerDay) then
      ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.MegaL')
    else
      ViewDialog.UnitsLabel.Caption := '('+ FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
  except on E: Exception do HandleError(E, OPNAME) end;
end;
procedure TOutputComplianceGridValidator.PopulateIFRMonthlyDeficitGridData(AIFRFeature: TIFRFeature; AData: TStrings);
const OPNAME = 'TOutputComplianceGridValidator.PopulateIFRMonthlyDeficitGridData';
var
  LIndex,
  LCount: integer;
  LComplyContainer,
  LCompliance : TStringList;
  LReferenceData : TStringList;
  LReferenceUnRankedData : TStringList;
  LDaysInMonth : double;
  LRequirement : double;
  LSupply : double;
  LDifference : double;
  LDifferenceAverage : array[1..12] of double;
  LAnnualDifferenceAverage : array of double;
  LGrandAnnualDifferenceAverage : double;

begin
  try
    LockWindowUpdate(FPanel.Handle);
    LCompliance := TStringList.Create;
    LComplyContainer:= TStringList.Create;
    try
      ClearGrid;
      ViewDialog.grdMonthlySupplyData.Visible         := True;
      ViewDialog.grdMonthlySupplyData.CellColor := clRed;
      if(AData.Count > 0) then
      begin
        ViewDialog.grdMonthlySupplyData.Cells[0,1] := FAppModules.Language.GetString('MonthGrid.Year');
        ViewDialog.grdMonthlySupplyData.Cells[13,1] := FAppModules.Language.GetString('MonthGrid.Average');
                ViewDialog.grdMonthlySupplyData.RowCount := AData.Count+3;
        SetLength(LAnnualDifferenceAverage,Adata.Count);
        LReferenceData := TStringList.Create;
        LReferenceUnRankedData := TStringList.Create;
        try
          if not AIFRFeature.GetNodeReferenceFlowData(LReferenceUnRankedData) then
            Exit;
          for LIndex := 1 to 12 do
          begin
            ViewDialog.grdMonthlySupplyData.Cells[LIndex,1] :=
            TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthNameByIndex[LIndex];
            LReferenceData.CommaText := LReferenceUnRankedData.CommaText;
            GetMonthData(LReferenceData,LIndex);
            LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).
                            CastRunConfigurationData.MonthDaysByIndex[LIndex];
            if AData.Count <> LReferenceData.Count then
              Exit;
            for LCount := 0 to LReferenceData.Count-1 do
            begin
              LRequirement := AIFRFeature.GetRequirementFlowFromReferenceFlow(LIndex,StrToFloat(LReferenceData[LCount]));
              LSupply := GetSupplyValueByYear(integer(LReferenceData.Objects[LCount]), LIndex,AData);
              LDifference := 0;
              if LRequirement >= LSupply then
                LDifference := LRequirement - LSupply;
              ViewDialog.grdMonthlySupplyData.Cells[0,LCount+2] := IntToStr(integer(LReferenceData.Objects[LCount]));
              ViewDialog.grdMonthlySupplyData.Cells[LIndex,LCount+2] := Format('%6.3f', [LDifference]);
              LDifferenceAverage[LIndex] := LDifferenceAverage[LIndex] + LDifference;
              LAnnualDifferenceAverage[LCount] := ((LDaysInMonth*LDifference));
            end;
            ViewDialog.grdMonthlySupplyData.Cells[0,ViewDialog.grdMonthlySupplyData.RowCount-1] :=
            FAppModules.Language.GetString('MonthGrid.Average');;
            ViewDialog.grdMonthlySupplyData.Cells[LIndex,ViewDialog.grdMonthlySupplyData.RowCount-1] :=
            Format('%6.3f', [LDifferenceAverage[LIndex]/LReferenceData.Count]);
          end;
          LGrandAnnualDifferenceAverage := 0;
          for LCount := 0 to LReferenceData.Count-1 do
          begin
            LGrandAnnualDifferenceAverage := LGrandAnnualDifferenceAverage + LAnnualDifferenceAverage[LCount]/364.25;
            ViewDialog.grdMonthlySupplyData.Cells[13,LCount+2] := Format('%6.3f', [LAnnualDifferenceAverage[LCount]/364.25]);
          end;
          ViewDialog.grdMonthlySupplyData.Cells[13,ViewDialog.grdMonthlySupplyData.RowCount-1] :=
          Format('%6.3f', [LGrandAnnualDifferenceAverage/LReferenceData.Count]);
        finally
          LReferenceData.Free;
          LReferenceUnRankedData.Free;
          Finalize(LAnnualDifferenceAverage);
        end;
      end;

      for LCount := 2 to ViewDialog.grdMonthlySupplyData.RowCount-1 do
      begin
        LCompliance.Clear;
        LCompliance.Add('N');
        for LIndex := 1 to 13 do
        begin
          LDifference := StrToFloat(Trim(ViewDialog.grdMonthlySupplyData.Cells[LIndex,LCount]));
          if LDifference > 0 then
            LCompliance.Add('Y')
          else
            LCompliance.Add('N');
        end;
        LComplyContainer.Add(LCompliance.CommaText);
      end;
      for LIndex := 1 to 2 do
        LComplyContainer.Insert(0,'N,N,N,N,N,N,N,N,N,N,N,N,N,N');
      ViewDialog.grdMonthlySupplyData.Coloring.Assign(LComplyContainer);

    finally
      LockWindowUpdate(0);
      LCompliance.Free;
      LComplyContainer.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComplianceGridValidator.PopulateChannelDemandData(AValues: TStringList; AChannel: IGeneralFlowChannel);
const OPNAME = 'TOutputComplianceGridValidator.PopulateChannelDemandData';
var
  LErrors                : string;
begin
  try
    AValues.Clear;
    if(AChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,ctDemandChannel,ctIrrigationBlockInflowChannel,ctIFRChannel]) then
    begin
      if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastDemandOutputData.GetChannelDemandValues(
         AValues,AChannel.ChannelNumber,LErrors) then
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

