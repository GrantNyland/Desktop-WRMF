{******************************************************************************}
{*  UNIT      : Contains the class TIFRFeatureValidator.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/11/27                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UIFRFeatureValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCLTee.Series,
  UIFRFeatures,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UIFRFeatureDialog;

type
  TMonthError = class(TObject)
  private
    FMonthError : array [1..12] of Boolean;
    function GetMonthError (AMonth : integer) : Boolean;
    procedure SetMonthError (AMonth : integer;
                             AError : Boolean);
  public
    procedure SetToFalse;
    property MonthError [AMonth : integer] : Boolean read GetMonthError write SetMonthError; default;
  end;

  TIFRFeatureValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FFeatureID : integer;
    FFileNameContainer : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure DoOnGraphMonthCbxSelect(Sender : TObject);
    procedure DoOnInflowIFRMonthCbxSelect(Sender : TObject);
    procedure DoOnRefDemandCbxSelect(Sender : TObject);
    procedure DoRefMonthsCbxSelect(Sender : TObject);
    procedure RePopulateDataViewer;
    procedure RePopulateLagInMonths(AIFRFeature : IIFRFeature);
    procedure RePopulateReferenceNodes;
    procedure CheckReferenceNodes;
    procedure RePopulateGrids;
    procedure ResizeGrids;
    procedure ValuesGridTopLeftChanged(Sender: TObject);
    procedure ExceedenceGridTopLeftChanged(Sender: TObject);
    procedure UpdateMARDisplay(AReservoirName: string);
    procedure PopulateFileNames(AFileNameContainer: TStringList);
    function  CalculateMAR(AFileNames: TStringList): double;
    procedure GetSelectedNodes(out ANodesNameList: TStringList);
    procedure CalculteMAROfSelectedNodes;

    procedure OnUpdateIFRFromReferenceInflowsClicked(Sender: TObject);
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure OnReferenceNodesSelectionChange(Sender : TObject);
    function ReferenceNodesSelectionSave : integer;
    function GetValuePercentile(AIFRFeature : TIFRFeature; const AYValue : double; const AMonth : integer): double;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnAfterPasteExceedenceGridColumnData(ASender : TObject);
    procedure OnAfterPasteValuesGridColumnData(ASender: TObject);
    procedure OnAfterPasteValuesGridData(ASender: TObject);
    procedure OnIFRExistsClick(Sender: TObject);
    procedure OnIFRLossClick(Sender: TObject);

    procedure UpdateFeatureName;
    procedure UpdateLagInMonths;
    procedure UpdateNumberOfPoints;
    procedure UpdateReferenceNodes;
    procedure UpdateSiteData;
    procedure UpdateIFRFeatureExists;
    procedure UpdateIFRLoss;
//    procedure UpdateCalculationOption;
    procedure UpdateInflow(AIndex : integer;
                           AMonth : integer;
                           AValue : string);
    procedure UpdateRelease(AIndex : integer;
                            AMonth : integer;
                            AValue : string);
    procedure UpdateExceedence(AIndex : integer;
                               AValue : string);
    procedure UpdateMonthlyIFRLossGrid(AIndex : integer; AValue : string);
    procedure ValidateFeatureName(AFeature : IIFRFeature);
    procedure ValidateLagInMonths (AFeature : IIFRFeature);
    procedure ValidateReferenceNodes (AFeature : IIFRFeature);
    procedure ValidateInflows (AFeature : IIFRFeature);
    procedure ValidateReleases (AFeature : IIFRFeature);
    procedure ValidatePointsCount(AFeature: IIFRFeature);
    procedure UnitOptionsRadioGroupOnClick(Sender: TObject);
    procedure rdgAnnualMonthlyOptionOnClick(Sender: TObject);
    procedure GetSortedData(AData: TStrings);

    procedure DisplayLegend(AChart : TAbstractChart;ASeriesA,ASeriesB : string);
    procedure GetSortedDataForAllMonths(AData: TStrings);
    procedure GetMonthData(AData: TStrings;AMonth : integer);
    function ConvertToMcM(AValue : double; AIndex : integer) : double;
    procedure GetSortedDataByMonth(AData: TStrings;AMonth : integer);
    procedure PopulateRequirementAndFlow(AIFRFeature : TIFRFeature);
    procedure PopulateReferenceFlow(AIFRFeature: TIFRFeature);
    procedure GetChartLegend(AChart: TAbstractChart);

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
    function IFRFeatureDialog : TIFRFeatureDialog;
    property FeatureID : integer read FFeatureID write FFeatureID;
  end;

implementation

uses
  VCLTee.TeEngine,
  VCLTee.Chart,
  Math,
  SysUtils,
  VCL.Graphics,
  ContNrs,
  VCLTee.TeeShape,
  UConstants,
  UUtilities,
  UDataSetType,
  UIFRDataObject,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UParameterData,
  UNetworkElementData,
  UPlanningModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TIFRFeatureValidator                                                       *}
{******************************************************************************}

procedure TIFRFeatureValidator.CreateMemberObjects;
const OPNAME = 'TIFRFeatureValidator.CreateMemberObjects';
var
  lPanel : TIFRFeatureDialog;

begin
  try
    inherited CreateMemberObjects;
    FFeatureID := -1;
    FPanel := TIFRFeatureDialog.Create(FPanelOwner,FAppModules);
    lPanel := IFRFeatureDialog;
    FFileNameContainer := TStringList.Create;
    with lPanel do
    begin
      SitesCbx.FieldProperty        := FAppModules.FieldProperties.FieldProperty('IFRSiteID');
      SitesCbx.OnEnter              := OnEditControlEnter;
      SitesCbx.OnExit               := OnEditControltExit;

      FeatureNameEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('IFRFeatureName');
      FeatureNameEdit.OnEnter       := OnEditControlEnter;
      FeatureNameEdit.OnExit        := OnEditControltExit;

      LagInMonthsCbx.FieldProperty  := FAppModules.FieldProperties.FieldProperty('LagInMonthsCount');
      LagInMonthsCbx.OnEnter        := OnEditControlEnter;
      LagInMonthsCbx.OnExit         := OnEditControltExit;

      NrOfPointsEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IFRPointsCount');
      NrOfPointsEdit.OnEnter        := OnEditControlEnter;
      NrOfPointsEdit.OnExit         := OnEditControltExit;

{      CalcOptionCbx.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IFRCalcOption');
      CalcOptionCbx.OnEnter        := OnEditControlEnter;
      CalcOptionCbx.OnExit         := OnEditControltExit;
      }

      UnitsOptionRadioGroup.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IFRUnitsOption');
      UnitsOptionRadioGroup.OnEnter        := OnEditControlEnter;
      UnitsOptionRadioGroup.OnExit         := OnEditControltExit;
      UnitsOptionRadioGroup.OnClick        := UnitOptionsRadioGroupOnClick;

      rdgAnnualMonthlyOption.FieldProperty  := FAppModules.FieldProperties.FieldProperty('IFRBaseOption');
      rdgAnnualMonthlyOption.OnEnter        := OnEditControlEnter;
      rdgAnnualMonthlyOption.OnExit         := OnEditControltExit;
      rdgAnnualMonthlyOption.OnClick        := rdgAnnualMonthlyOptionOnClick;


      ReferenceNodesCheckLbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('RefNodeNumber');
      ReferenceNodesCheckLbx.OnEnter       := OnEditControlEnter;
      ReferenceNodesCheckLbx.OnClickCheck  := OnReferenceNodesSelectionChange;

      ExceedenceGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Month'));
      ExceedenceGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ExceedencePercentage'));
      ExceedenceGrid.OnBeforeCellChange     := OnStringGridCellDataHasChanged;
      ExceedenceGrid.OnEnter                := OnEditControlEnter;
      ExceedenceGrid.OnColEnter             := OnStringGridColEnter;
      ExceedenceGrid.OnTopLeftChanged       := ExceedenceGridTopLeftChanged;
      ExceedenceGrid.ShowGridPopupMenu      := True;
      ExceedenceGrid.OnAfterPasteColumnData := OnAfterPasteExceedenceGridColumnData;

      ValuesGrid.OnBeforeCellChange             := OnStringGridCellDataHasChanged;
      ValuesGrid.OnColEnter                     := OnStringGridColEnter;
      ValuesGrid.OnTopLeftChanged               := ValuesGridTopLeftChanged;
      ValuesGrid.OnEnter                        := OnEditControlEnter;
      ValuesGrid.ShowGridPopupMenu              := True;
      ValuesGrid.AllowPasteFromExcel            := True;
      ValuesGrid.OnAfterPasteColumnData         := OnAfterPasteValuesGridColumnData;
      ValuesGrid.OnAfterPasteColumnsAndRowsData := OnAfterPasteValuesGridData;
      ValuesGrid.OnPasteFromExcel               := OnAfterPasteValuesGridData;

      GraphMonthCbx.OnSelect         := DoOnGraphMonthCbxSelect;
      InflowIFRMonthCbx.OnSelect     := DoOnInflowIFRMonthCbxSelect;
      //RefDemandCbx.OnSelect          := DoOnRefDemandCbxSelect;
      RefMonthsCbx.OnSelect          := DoRefMonthsCbxSelect;
      chkboxUpdateIFRFromReferenceInflows.OnClick :=  OnUpdateIFRFromReferenceInflowsClicked;

      ChkboxFIFRFeatureExists.FieldProperty       :=  FAppModules.FieldProperties.FieldProperty('IFRStatusIndicator');
      chkboxFIFRFeatureExists.OnClick             :=  OnIFRExistsClick;
      ChkboxFIFRFeatureExists.OnEnter             :=  OnEditControlEnter;
      ChkboxFIFRFeatureExists.OnExit              :=  OnEditControltExit;

      IFRLoss.FieldProperty       :=  FAppModules.FieldProperties.FieldProperty('IFRLoss');
      IFRLoss.OnClick             :=  OnIFRLossClick;
      IFRLoss.OnEnter             :=  OnEditControlEnter;
      IFRLoss.OnExit              :=  OnEditControltExit;

      MonthlyIFRLossGrid.OnBeforeCellChange     := OnStringGridCellDataHasChanged;
      MonthlyIFRLossGrid.OnEnter                := OnEditControlEnter;
      MonthlyIFRLossGrid.OnColEnter             := OnStringGridColEnter;


  end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.DestroyMemberObjects;
const OPNAME = 'TIFRFeatureValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
    FFileNameContainer.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.Initialise: boolean;
const OPNAME = 'TIFRFeatureValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex      := 0;
   // IFRFeatureDialog.MonthlyIFRLossTabsheet.Visible       := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.LanguageHasChanged: boolean;
const OPNAME = 'TIFRFeatureValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('NetworkFeatures.IFRFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.ClearDataViewer;
const OPNAME = 'TIFRFeatureValidator.ClearDataViewer';
var
  lPanel : TIFRFeatureDialog;
  lMonth : integer;
  lIndex : integer;
begin
  inherited ClearDataViewer;
  try
    lPanel := IFRFeatureDialog;
    with lPanel do
    begin
      FeatureNameEdit.SetFieldValue('');
      LagInMonthsCbx.ItemIndex := -1;
      LagInMonthsCbx.Items.Clear;
      NrOfPointsEdit.Text := '-1';
      ReferenceNodesCheckLbx.Items.Clear;
      for lIndex := 0 to ExceedenceGrid.RowCount - 1 do
      begin
        ExceedenceGrid.Cells[0, lIndex] := '';
        ExceedenceGrid.Cells[1, lIndex] := '-1';
      end;
      for lMonth := 0 to 11 do
      begin
        MonthsGrid.Cells[lMonth, 0] := '-1';
        ValuesGrid.Cells[lMonth*2, 0] := 'xx';
        ValuesGrid.Cells[lMonth*2+1, 0] := 'yy';
        for lIndex := 1 to ValuesGrid.RowCount - 1 do
        begin
          ValuesGrid.Cells[lMonth*2, lIndex] := '-1.0';
          ValuesGrid.Cells[lMonth*2+1, lIndex] := '-1.0';
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.PopulateDataViewer;
const OPNAME = 'TIFRFeatureValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtIFRFeature);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.RePopulateDataViewer;
const OPNAME = 'TIFRFeatureValidator.RePopulateDataViewer';
var
  lIFRFeature    : TIFRFeature;
  lMonths        : TMonthNamesArray;
  lCol           : integer;
  LIndex         : integer;
  LIFRSite       : TIFRSiteDataObject;
  LIFRSiteList   : TIFRSiteDataList;
begin
  lMonths := nil;
  try
    IFRFeatureDialog.LanguageHasChanged;
    if (FFeatureID >= 0) then
    begin
      lMonths := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastRunConfigurationData.MonthNamesArray;
      lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                        CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByID(FFeatureID);
      if (lIFRFeature <> nil) then
      begin
        with IFRFeatureDialog do
        begin
          SitesCbx.Items.Clear;
          LIFRSiteList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.IFRSiteList;
          for LIndex := 0 to LIFRSiteList.IFRSiteDataCount-1 do
          begin
            LIFRSite := LIFRSiteList.IFRSiteDataByIndex[LIndex];
            if(LIFRSite <> nil) then
              SitesCbx.Items.AddObject(LIFRSite.SiteName,TObject(LIFRSite.SiteIdentifier));
          end;
          LIFRSite := LIFRSiteList.IFRSiteDataByIdentifier[lIFRFeature.IFRSiteID];
          if(LIFRSite <> nil) then
            SitesCbx.ItemIndex := SitesCbx.Items.IndexOf(LIFRSite.SiteName);
          FeatureNameEdit.SetFieldValue(lIFRFeature.FeatureName);
          RePopulateLagInMonths(lIFRFeature);
          NrOfPointsEdit.SetFieldValue(lIFRFeature.NrOfInflowIFRPoints);
          for lCol := 1 to 12 do
            MonthsGrid.Cells[lCol-1, 0]   := lMonths[lCol];
          ResizeGrids;
          RePopulateReferenceNodes;
          ChkboxFIFRFeatureExists.Checked := (lIFRFeature.IFRFeatureExists=1);
          IFRLoss.Checked := (lIFRFeature.IFRLoss=1);
          if not IFRLoss.Checked then
            MonthlyIFRLossTabsheet.PageControl := nil
          else
          begin
            MonthlyIFRLossTabsheet.Visible := IFRLoss.Checked;
            MonthlyIFRLossTabsheet.PageControl := IFRPageControl;
            IFRPageControl.ActivePage := MonthlyIFRLossTabsheet;
            MonthlyIFRLossGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MonthlyIFRLoss'));
            for lCol := 1 to 12 do
            begin
              MonthlyIFRLossGrid.Cells[lCol-1, 0] := lMonths[lCol];
              MonthlyIFRLossGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('MonthlyIFRLoss'));
              MonthlyIFRLossGrid.Cells[lCol-1, 1] := FormatFloat('##0.00',lIFRFeature.MonthlyIFRLossByIndex[lCol-1]);
            end;
          end;
       end;
      end;
    end;
    IFRFeatureDialog.GraphMonthCbx.Items.Clear;
    IFRFeatureDialog.InflowIFRMonthCbx.Items.Clear;
    IFRFeatureDialog.RefMonthsCbx.Items.Clear;
    IFRFeatureDialog.GraphMonthCbx.Items.AddObject('ALL',TObject(0));
    for LIndex := 1 to 12 do
    begin
      if(LIndex > High(LMonths)) then Break;
      IFRFeatureDialog.GraphMonthCbx.Items.AddObject(LMonths[LIndex],TObject(LIndex));
      IFRFeatureDialog.InflowIFRMonthCbx.Items.AddObject(LMonths[LIndex],TObject(LIndex));
      IFRFeatureDialog.RefMonthsCbx.Items.AddObject(LMonths[LIndex],TObject(LIndex));
    end;

    IFRFeatureDialog.chkboxUpdateIFRFromReferenceInflows.Checked := TYieldModelDataObject(FAppModules.Model.ModelData).
                                                                    CastNetworkFeaturesData.CastIFRFeatureList.UpdateIFRFromReferenceInflows;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.ConvertToMcM(AValue : double; AIndex : integer) : double;
const OPNAME = 'TIFRFeatureValidator.ConvertToMcM';
var
  LDaysInMonth : double;
begin
  Result :=  AValue;
  try
    LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[AIndex];
    case IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex of
      1 : Result := (AValue*LDaysInMonth*24*60*60)/Power(10,6);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRFeatureValidator.GetMonthData(AData: TStrings;AMonth : integer);
const OPNAME = 'TIFRFeatureValidator.GetMonthData';
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

procedure TIFRFeatureValidator.GetSortedDataForAllMonths(AData: TStrings);
const OPNAME = 'TIFRFeatureValidator.GetSortedDataForAllMonths';
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


procedure TIFRFeatureValidator.DisplayLegend(AChart : TAbstractChart;ASeriesA,ASeriesB : string);
const OPNAME = 'TIFRFeatureValidator.DisplayLegend';
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


procedure TIFRFeatureValidator.GetSortedDataByMonth(AData: TStrings;AMonth : integer);
const OPNAME = 'TIFRFeatureValidator.GetSortedDataByMonth';
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


procedure TIFRFeatureValidator.PopulateRequirementAndFlow(AIFRFeature : TIFRFeature);
const OPNAME = 'TIFRFeatureValidator.PopulateRequirementAndFlow';
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
  LDisplayMonth : integer;
begin
  try
    IFRFeatureDialog.ReferenceFlowVsDemand.Visible := True;
    IFRFeatureDialog.ReferenceFlowVsDemand.Legend.Visible := False;
    IFRFeatureDialog.ReferenceFlowVsDemand.BottomAxis.Title.Caption :=
                           FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
    if (IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
      IFRFeatureDialog.ReferenceFlowVsDemand.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRAndRefFlow'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if (IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 0) then
      IFRFeatureDialog.ReferenceFlowVsDemand.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedIFRAndRefFlow'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);

    IFRFeatureDialog.ReferenceFlowVsDemand.BottomAxis.SetMinMax(0,100);
    IFRFeatureDialog.ReferenceFlowVsDemand.BottomAxis.Increment := 10;
    IFRFeatureDialog.ReferenceFlowVsDemand.Legend.Visible := True;
    IFRFeatureDialog.ReferenceFlowVsDemand.Title.Text.Text := IFRFeatureDialog.RefMonthsCbx.Text;
    LDisplayMonth := IFRFeatureDialog.RefMonthsCbx.ItemIndex;
    IFRFeatureDialog.RefLineSeries.Clear;
    IFRFeatureDialog.DemLineSeries.Clear;
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
          if (LDisplayMonth > 0) then
          begin
            GetSortedDataByMonth(LReferenceFlowData,LDisplayMonth);
            for LCount := 0 to LReferenceFlowData.Count-1 do
            begin
              LRequirement := AIFRFeature.GetRequirementFlowFromReferenceFlow(LDisplayMonth,StrToFloat(LReferenceFlowData[LCount]));
              LPercentile := (LCount/LReferenceFlowData.Count)*100;
              if (LRequirement <> NullFloat) then
              begin
                LRequirement := ConvertToMcM(LRequirement,LDisplayMonth);
                IFRFeatureDialog.DemLineSeries.AddXY(LPercentile,LRequirement);
                IFRFeatureDialog.DemLineSeries.Title := 'Demand';
                //FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR');
              end;
              if (StrToFloat(LReferenceFlowData[LCount]) <> NullFloat) then
              begin
                LReference := StrToFloat(LReferenceFlowData[LCount]);
                LReference := ConvertToMcM(LReference,LDisplayMonth);
                IFRFeatureDialog.RefLineSeries.AddXY(LPercentile,LReference);
                IFRFeatureDialog.RefLineSeries.Title :=
                FAppModules.Language.GetString('OutputReview.OutputGraphReferenceFlow');
              end;
            end;
          end
          else
          if (LDisplayMonth = 0) then
          begin
            IFRFeatureDialog.RefLineSeries.Clear;
            IFRFeatureDialog.DemLineSeries.Clear;

            IFRFeatureDialog.DemLineSeries.Visible := True;
            IFRFeatureDialog.RefLineSeries.Visible := True;
            IFRFeatureDialog.RefLineSeries.Pen.Width := 2;
            IFRFeatureDialog.DemLineSeries.Pen.Width := 2;
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
              IFRFeatureDialog.DemLineSeries.AddXY(LPercentile,LRequirement);
              IFRFeatureDialog.DemLineSeries.Title := 'Demand';//FAppModules.Language.GetString('OutputReview.OutputGraphRequiredIFR');
              IFRFeatureDialog.RefLineSeries.AddXY(LPercentile,LYValue);
              IFRFeatureDialog.RefLineSeries.Title := FAppModules.Language.GetString('OutputReview.OutputGraphReferenceFlow');
            end;
          end;
        end;
        DisplayLegend(IFRFeatureDialog.ReferenceFlowVsDemand,FAppModules.Language.GetString('OutputReview.OutputGraphReferenceFlow'),'Demand');
      finally
        LReferenceFlowData.Free;
        LMonthData.Free;
        LRequiredData.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TIFRFeatureValidator.ValuesGridTopLeftChanged(Sender: TObject);
const OPNAME = 'TIFRFeatureValidator.ValuesGridTopLeftChanged';
begin
  try
    with IFRFeatureDialog do
    begin
      if (ValuesGrid.LeftCol mod 2 = 1) then
      begin
        if (ValuesGrid.Col > 11) then
          ValuesGrid.LeftCol := ValuesGrid.LeftCol + 1
        else
          ValuesGrid.LeftCol := ValuesGrid.LeftCol - 1;
      end;
      MonthsGrid.LeftCol := ValuesGrid.LeftCol div 2;
      ExceedenceGrid.TopRow  := ValuesGrid.TopRow - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.ExceedenceGridTopLeftChanged(Sender: TObject);
const OPNAME = 'TIFRFeatureValidator.ExceedenceGridTopLeftChanged';
begin
  try
    with IFRFeatureDialog do
    begin
      ValuesGrid.TopRow := ExceedenceGrid.TopRow + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.ResizeGrids;
const OPNAME = 'TIFRFeatureValidator.ResizeGrids';
var
  sCount   : string;
  nCount   : integer;
  nIndex   : integer;
  bSize    : boolean;
begin
  try
    with IFRFeatureDialog do
    begin
      nCount := 0;
      bSize  := FALSE;
      sCount := Trim(NrOfPointsEdit.Text);
      if (sCount <> '') then
      begin
        nCount := StrToInt(sCount);
        if (nCount >= 0) then
        begin
          bSize := TRUE;
          if (nCount > 10) then
            ValuesGrid.Width := 712
          else
            ValuesGrid.Width := 712 - 16;
          ExceedenceGrid.RowCount := nCount;
          ValuesGrid.RowCount := nCount+1;
          if (nCount > 0) then
            ValuesGrid.FixedRows := 1;
          ValuesGrid.Reset;
          for nIndex := 1 to 12 do
          begin
            ValuesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('IFRVariables'));
            ValuesGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('IFRReleaseVariables'));
          end;
          for nIndex := 1 to nCount do
            ExceedenceGrid.Cells[0, nIndex-1] := IntToStr(nIndex);
        end;
      end;
      if (bSize) then
      begin
        if (nCount > 0) then
        begin
          ValuesGrid.Visible     := TRUE;
          ExceedenceGrid.Visible := TRUE;
          RePopulateGrids;
        end
        else
        begin
          ValuesGrid.Visible     := FALSE;
          ExceedenceGrid.Visible := FALSE;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.RePopulateLagInMonths(AIFRFeature : IIFRFeature);
const OPNAME = 'TIFRFeatureValidator.RePopulateLagInMonths';
var
  lValue         : integer;
  lKeyValues     : string;
  lHasChanges    : boolean;
  lFieldIndex    : string;
  lFieldProperty : TAbstractFieldProperty;
  lIFRFeature    : IIFRFeature;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
      if (lIFRFeature <> nil) then
      begin
        with IFRFeatureDialog do
        begin
          lFieldIndex    := '';
          lFieldProperty := LagInMonthsCbx.FieldProperty;
          lKeyValues     := lIFRFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
          lHasChanges    := FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
          LagInMonthsCbx.HasChanges := lHasChanges;

          for lValue := -12 to 12 do
            LagInMonthsCbx.Items.Add(IntToStr(lValue));
          LagInMonthsCbx.ItemIndex := AIFRFeature.LagMonths + 12;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.RePopulateReferenceNodes;
const OPNAME = 'TIFRFeatureValidator.RePopulateReferenceNodes';
var
  lIndex         : integer;
  lReservoirList : IReservoirDataList;
  lReservoirData : IReservoirData;
  lReservoir     : IReservoirConfigurationData;
  LIFRFeature    : IIFRFeature;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
      if (lIFRFeature <> nil) then
      begin
        lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ReservoirList;
        if (lReservoirList <> nil) then
        begin
          with IFRFeatureDialog do
          begin
            ReferenceNodesCheckLbx.Items.Clear;
            for lIndex := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
            begin
              lReservoirData := lReservoirList.ReservoirOrNodeByIndex[lIndex];
              lReservoir     := lReservoirData.ReservoirConfigurationData;
              if (lReservoir.NodeType in NodeWithInflowAndReservoirSet) then
              begin
                ReferenceNodesCheckLbx.Items.AddObject
                    ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                     TObject(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
              end;
            end;
          end;
          CheckReferenceNodes;
          CalculteMAROfSelectedNodes;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.CheckReferenceNodes;
const OPNAME = 'TIFRFeatureValidator.CheckReferenceNodes';
var
  lIndex         : integer;
  lItemIndex     : integer;
  lReservoirList : IReservoirDataList;
  lRefNodeNr     : integer;
  lReferenceNode : IReservoirData;
  lIFRFeature    : IIFRFeature;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
      lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
      if (lIFRFeature <> nil) then
      begin
        with IFRFeatureDialog do
        begin
          for lIndex := 0 to lIFRFeature.ReferenceNodeNumbersCount - 1 do
          begin
            lRefNodeNr     := lIFRFeature.ReferenceNodeNumberByIndex[lIndex];
            lReferenceNode := lReservoirList.ReservoirOrNodeByIdentifier[lRefNodeNr];
            if (lReferenceNode <> nil) then
            begin
              lItemIndex := ReferenceNodesCheckLbx.Items.IndexOfObject(TObject(lRefNodeNr));
              if  (lItemIndex <> -1) then
                ReferenceNodesCheckLbx.Checked[lItemIndex] := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.RePopulateGrids;
const OPNAME = 'TIFRFeatureValidator.RePopulateGrids';
var
  lIFRFeature         : IIFRFeature;
  lCol                : integer;
  lRow                : integer;
  lValue              : double;
  lKeyValues          : string;
  lHasChanges         : boolean;
  lFieldIndex         : string;
  LDaysInMonth        : double;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    if (FFeatureID >= 0) then
    begin
      lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
      if (lIFRFeature <> nil) then
      begin
        with IFRFeatureDialog do
        begin
          for lRow := 1 to ExceedenceGrid.RowCount {lIFRFeature.NrOfInflowIFRPoints} do
          begin
            ExceedenceGrid.Cells[0, lRow-1] := IntToStr(lRow);
            lValue := lIFRFeature.ExceedencePercentageByIndex[lRow];
            if (lValue <> NullFloat) then
              ExceedenceGrid.Cells[1, lRow-1] := Format('%6.2f', [lValue])
            else
              ExceedenceGrid.Cells[1, lRow-1] := '';
          end;
          for lCol := 1 to 12 do
          begin
            LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[lCol];
            ValuesGrid.Cells[lCol*2-2, 0] := 'Inflow';
            ValuesGrid.Cells[lCol*2-1, 0] := 'IFR';
            for lRow := 1 to ValuesGrid.RowCount-1 do
            begin
              lFieldIndex    := IntToStr(lRow) + ',' + IntToStr(lCol);
              lFieldProperty := ValuesGrid.FieldProperty(lCol);
              lKeyValues     := lIFRFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              lHasChanges    := FAppModules.Changes.HasParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              ValuesGrid.HasChanges[lCol, lRow] := lHasChanges;

              lValue := lIFRFeature.InflowByIndexAndMonth[lRow, lCol];
              if(UnitsOptionRadioGroup.ItemIndex = 1) then
                lValue := (lValue *(60*60*24*LDaysInMonth))/ Power(10, 6);
              if (lValue <> NullFloat) then
                ValuesGrid.Cells[lCol*2-2, lRow] := Format('%6.3f', [lValue])
              else
                ValuesGrid.Cells[lCol*2-2, lRow] := '';

              lValue := lIFRFeature.ReleaseByIndexAndMonth[lRow, lCol];
              if(UnitsOptionRadioGroup.ItemIndex = 1) then
                lValue := (lValue *(60*60*24*LDaysInMonth))/ Power(10, 6);
              if (lValue <> NullFloat) then
                ValuesGrid.Cells[lCol*2-1, lRow] := Format('%6.3f', [lValue])
              else
                ValuesGrid.Cells[lCol*2-1, lRow] := '';
            end;
          end;
        end;
      end;
      DoContextValidation(dvtIFRFeatureInflows);
      DoContextValidation(dvtIFRFeatureReleases);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.SaveState: boolean;
const OPNAME = 'TIFRFeatureValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.IFRFeatureDialog : TIFRFeatureDialog;
const OPNAME = 'TIFRFeatureValidator.IFRFeatureDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TIFRFeatureDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TIFRFeatureValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.StudyHasChanged: boolean;
const OPNAME = 'TIFRFeatureValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TIFRFeatureValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TIFRFeatureValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with IFRFeatureDialog do
    begin
      if ((Sender = FeatureNameEdit) AND
          (FeatureNameEdit.HasValueChanged)) then
        UpdateFeatureName
      else
      if ((Sender = LagInMonthsCbx) AND
          (LagInMonthsCbx.HasValueChanged)) then
        UpdateLagInMonths
      else
      if ((Sender = NrOfPointsEdit) AND
          (NrOfPointsEdit.HasValueChanged)) then
        UpdateNumberOfPoints;
      if ((Sender = SitesCbx) AND
          (SitesCbx.HasValueChanged)) then
        UpdateSiteData;
{      else
      if ((Sender = CalcOptionCbx) AND
          (CalcOptionCbx.HasValueChanged)) then
        UpdateCalculationOption;
        }
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateFeatureName;
const OPNAME = 'TIFRFeatureValidator.UpdateFeatureName';
var
  lFeature  : IIFRFeature;
  lMessage  : string;
begin
  try
    lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (lFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
          FeatureNameEdit.FieldProperty.FieldName,
          FeatureNameEdit.Text,lMessage)) then
        begin
          FeatureNameEdit.FieldValidationError := lMessage;
          lFeature.FeatureName := Trim(FeatureNameEdit.Text);
          FeatureNameEdit.SetFieldValue(lFeature.FeatureName);
          DoContextValidation(dvtIFRFeatureName);
        end
        else
          FeatureNameEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateLagInMonths;
const OPNAME = 'TIFRFeatureValidator.UpdateLagInMonths';
var
  lIFRFeature  : IIFRFeature;
  lLagInMonths : integer;
  lMessage     : string;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        lLagInMonths := LagInMonthsCbx.ItemIndex - 12;
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            LagInMonthsCbx.FieldProperty.FieldName,
            IntToStr(lLagInMonths),lMessage)) then
        begin
          lIFRFeature.LagMonths    := lLagInMonths;
          LagInMonthsCbx.ItemIndex := lIFRFeature.LagMonths + 12;
        end
        else
          LagInMonthsCbx.ValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateNumberOfPoints;
const OPNAME = 'TIFRFeatureValidator.UpdateNumberOfPoints';
var
  lIFRFeature : IIFRFeature;
  lMessage    : string;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            NrOfPointsEdit.FieldProperty.FieldName,
            NrOfPointsEdit.Text, lMessage)) then
        begin
          NrOfPointsEdit.FieldValidationError := lMessage;
          lIFRFeature.NrOfInflowIFRPoints := StrToInt(Trim(NrOfPointsEdit.Text));
          NrOfPointsEdit.SetFieldValue(lIFRFeature.NrOfInflowIFRPoints);
          DoContextValidation(dvtPointsCount);
          ResizeGrids;
          RePopulateGrids;
        end
        else
          NrOfPointsEdit.FieldValidationError := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateReferenceNodes;
const OPNAME = 'TIFRFeatureValidator.UpdateReferenceNodes';
var
  lIFRFeature     : IIFRFeature;
  lIndex          : integer;
  lReferenceNodes : TStringList;
  lNewNode        : IReservoirData;
  lMessage        : string;
  LChannelNr      : integer;
  IReservoirList  : IReservoirDataList;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        IReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList;

        lReferenceNodes := TStringList.Create;
        lReferenceNodes.Clear;
        for lIndex := 0 to ReferenceNodesCheckLbx.Items.Count - 1 do
        begin
          if (ReferenceNodesCheckLbx.Checked[lIndex]) then
          begin
            LChannelNr := integer(ReferenceNodesCheckLbx.Items.Objects[lIndex]);
            lNewNode   := IReservoirList.ReservoirOrNodeByIdentifier[LChannelNr];
            if (lNewNode <> nil) then
              lReferenceNodes.Add(IntToStr(lNewNode.ReservoirConfigurationData.ReservoirIdentifier));
          end;
        end;

        lIFRFeature.ReferenceNodeNumbers := lReferenceNodes.CommaText;
        RePopulateDataViewer;

        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ReferenceNodeCount', IntToStr(lReferenceNodes.Count), lMessage)) then
        begin
          ReferenceNodesCheckLbx.InValidationError := False;
          ReferenceNodesCheckLbx.ShowErrorState(FALSE);
          CheckReferenceNodes;
          DoContextValidation(dvtIFRFeatureReferenceNodes);
        end
        else
        begin
          ReferenceNodesCheckLbx.InValidationError := False;
          ReferenceNodesCheckLbx.ValidationError := lMessage;
          ReferenceNodesCheckLbx.ShowErrorState(TRUE);
        end;
        FreeAndNil(lReferenceNodes);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TIFRFeatureValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with IFRFeatureDialog do
    begin
      if (ValuesGrid = ASender) then
      begin
        if ((ACol mod 2 = 0) AND
            (NOT ValuesGrid.HasChanges[ACol,ARow])) then
          UpdateInflow(ARow, (ACol div 2) + 1, Trim(ValuesGrid.Cells[ACol, ARow]))
        else
          UpdateRelease(ARow, (ACol div 2) + 1 , Trim(ValuesGrid.Cells[ACol, ARow]));
      end
      else if ((ExceedenceGrid = ASender) AND (ACol > 0)) then
        UpdateExceedence(ARow+1, Trim(ExceedenceGrid.Cells[ACol, ARow]))
      else if ((MonthlyIFRLossGrid = ASender) AND (ARow > 0)) then
        UpdateMonthlyIFRLossGrid(ACol, Trim(MonthlyIFRLossGrid.Cells[ACol, ARow]));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateInflow(AIndex : integer;
                                            AMonth : integer;
                                            AValue : string);
const OPNAME = 'TIFRFeatureValidator.UpdateInflow';
var
  lIFRFeature : IIFRFeature;
  lValue      : double;
  lMessage    : string;
  LDaysInMonth        : double;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        ValuesGrid.ValidationError[AMonth*2-2, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'IFRVariables', AValue, lMessage, AIndex, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          if(IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
          begin
            LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[AMonth];
            lValue := (lValue /(60*60*24*LDaysInMonth))* Power(10, 6);
          end;
          lIFRFeature.InflowByIndexAndMonth[AIndex, AMonth] := lValue;
          RePopulateGrids;
          DoContextValidation(dvtIFRFeatureInflows);
        end
        else
          ValuesGrid.ValidationError[AMonth*2-2, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateRelease(AIndex : integer;
                                             AMonth : integer;
                                             AValue : string);
const OPNAME = 'TIFRFeatureValidator.UpdateRelease';
var
  lIFRFeature : IIFRFeature;
  lValue      : double;
  lMessage    : string;
  LDaysInMonth        : double;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        ValuesGrid.ValidationError[AMonth*2-1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'IFRReleaseVariables', AValue, lMessage, AIndex, AMonth)) then
        begin
          lValue := StrToFloat(AValue);
          if(IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
          begin
            LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[AMonth];
            lValue := (lValue /(60*60*24*LDaysInMonth))* Power(10, 6);
          end;
          lIFRFeature.ReleaseByIndexAndMonth[AIndex, AMonth] := lValue;
          RePopulateGrids;
          DoContextValidation(dvtIFRFeatureReleases);
        end
        else
          ValuesGrid.ValidationError[AMonth*2-1, AIndex, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateExceedence(AIndex : integer;
                                                AValue : string);
const OPNAME = 'TIFRFeatureValidator.UpdateExceedence';
var
  lIFRFeature : IIFRFeature;
  lValue      : double;
  lMessage    : string;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        ExceedenceGrid.ValidationError[1, AIndex-1, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ExceedencePercentage', AValue, lMessage, AIndex)) then
        begin
          lValue := StrToFloat(AValue);
          lIFRFeature.ExceedencePercentageByIndex[AIndex] := lValue;
          RePopulateDataViewer;
          DoContextValidation(dvtIFRFeature);
        end
        else
          ExceedenceGrid.ValidationError[1, AIndex-1, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TMonthError }

function TMonthError.GetMonthError(AMonth: integer): Boolean;
const OPNAME = 'TMonthError.GetMonthError';
begin
  Result := FMonthError[AMonth];
end;

procedure TMonthError.SetMonthError(AMonth: integer; AError: Boolean);
const OPNAME = 'TMonthError.SetMonthError';
begin
  FMonthError[AMonth] := AError;
end;

procedure TMonthError.SetToFalse;
const OPNAME = 'TMonthError.SetToFalse';
var
  lMonth : integer;
begin
  for lMonth := 1 to 12 do
    FMonthError[lMonth] := FALSE;
end;

procedure TIFRFeatureValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TIFRFeatureValidator.DoContextValidation';
var
  lFeature     : IIFRFeature;
  lFeatureList : IIFRFeatureList;
begin
  try
    FAllErrorMessages.Clear;
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.IFRFeatureList;
      lFeature     := lFeatureList.MonthlyIFRFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep1, dvtIFRFeatureName]) then
          ValidateFeatureName(lFeature);
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep1, dvtIFRFeatureLagInMonths]) then
          ValidateLagInMonths(lFeature);
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep1, dvtIFRFeatureReferenceNodes]) then
          ValidateReferenceNodes(lFeature);
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep2, dvtIFRFeatureInflows]) then
          ValidateInflows(lFeature);
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep2, dvtIFRFeatureReleases]) then
          ValidateReleases(lFeature);
        if (AValidationType in [dvtIFRFeature, dvtIFRFeatureWizardStep1, dvtPointsCount]) then
          ValidatePointsCount(lFeature);

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TIFRFeatureValidator.DetermineWizardStatus';
var
  lFeature       : IIFRFeature;
  lFeatureList   : IIFRFeatureList;
  lIFRVariables  : TAbstractFieldProperty;
  lNotZero       : Boolean;
  lIndex         : integer;
  lMonth         : integer;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FFeatureID >= 0) then
    begin
      lFeatureList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkFeaturesData.IFRFeatureList;
      lFeature := lFeatureList.MonthlyIFRFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        case ASequence of
        1 :
          begin
            DoContextValidation(dvtIFRFeatureWizardStep1);
            if (lFeature.ReferenceNodeNumbersCount > 0) then
            begin
              Result := 1;
              if (FAllErrorMessages.Count = 0) then
                Result := 2;
            end;
          end;
        2 :
          begin
            DoContextValidation(dvtIFRFeatureWizardStep2);
            lIFRVariables := FAppModules.FieldProperties.FieldProperty('IFRVariables');
            lNotZero := FALSE;
            lIndex   := lIFRVariables.ArrayLow;
            while ((NOT lNotZero) AND (lIndex <= lFeature.NrOfInflowIFRPoints)) do
            begin
              lMonth := lIFRVariables.ArrayLowDimTwo;
              while ((NOT lNotZero) AND (lMonth <= lIFRVariables.ArrayHighDimTwo)) do
              begin
                if (lFeature.InflowByIndexAndMonth[lIndex, lMonth] > 0) then
                  lNotZero := TRUE
                else
                  lMonth := lMonth + 1;
              end;
              lIndex := lIndex + 1;
            end;
            if (lNotZero) then
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

procedure TIFRFeatureValidator.ValidateInflows(AFeature: IIFRFeature);
const OPNAME = 'TIFRFeatureValidator.ValidateInflows';
var
  lMonth     : integer;
  lErrorCols : TStringlist;
  lErrors    : TStringlist;
begin
  try
    if (AFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrors    := TStringList.Create;
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage,'Inflows')) then
        begin
          for lMonth := 1 to 12 do
            ValuesGrid.ValidationError[lMonth*2-2, 0, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrors, lErrorCols);
          for lMonth := 1 to 12 do
          begin
            if (lErrorCols.IndexOf(IntToStr(lMonth)) >= 0) then
              ValuesGrid.ValidationError[lMonth*2-2, 0, gveColContext] := lErrors.Text
            else
              ValuesGrid.ValidationError[lMonth*2-2, 0, gveColContext] := '';
          end;
          FAllErrorMessages.AddStrings(lErrors);
        end;
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.ValidateReleases(AFeature: IIFRFeature);
const OPNAME = 'TIFRFeatureValidator.ValidateReleases';
var
  lMonth     : integer;
  lErrorCols : TStringlist;
  lErrors    : TStringList;
begin
  try
    if (AFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        lErrorCols := TStringlist.Create;
        lErrors    := TStringlist.Create;
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'Releases')) then
        begin
          for lMonth := 1 to 12 do
            ValuesGrid.ValidationError[lMonth*2-1, 0, gveColContext] := '';
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrors,lErrorCols);
          for lMonth := 1 to 12 do
          begin
            if (lErrorCols.IndexOf(IntToStr(lMonth)) >= 0) then
              ValuesGrid.ValidationError[lMonth*2-1, 0, gveColContext] := lErrors.Text
            else
              ValuesGrid.ValidationError[lMonth*2-1, 0, gveColContext] := '';
          end;
          FAllErrorMessages.AddStrings(lErrors);
        end;
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.ValidateReferenceNodes(AFeature: IIFRFeature);
const OPNAME = 'TIFRFeatureValidator.ValidateReferenceNodes';
begin
  try
    if (AFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        FErrorMessage := '';
        if (AFeature.Validate(FErrorMessage, 'ReferenceNodes')) then
          ReferenceNodesCheckLbx.InValidationError := FALSE
        else
        begin
          ReferenceNodesCheckLbx.InValidationError := FALSE;
          ReferenceNodesCheckLbx.ValidationError := FErrorMessage;
          ReferenceNodesCheckLbx.ShowErrorState(TRUE);
          FAllErrorMessages.Add(Trim(FErrorMessage));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.ValidateFeatureName(AFeature: IIFRFeature);
const OPNAME = 'TIFRFeatureValidator.ValidateFeatureName';
begin
  try
    with IFRFeatureDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'FeatureName')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
      FeatureNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.ValidateLagInMonths(AFeature: IIFRFeature);
const OPNAME = 'TIFRFeatureValidator.ValidateLagInMonths';
begin
  try
    with IFRFeatureDialog do
    begin
      FErrorMessage := '';
      if (AFeature.Validate(FErrorMessage, 'LagInMonths')) then
      begin
        LagInMonthsCbx.InValidationError := FALSE;
        LagInMonthsCbx.ShowErrorState(FALSE);
      end
      else
      begin
        LagInMonthsCbx.InValidationError := TRUE;
        LagInMonthsCbx.ValidationError := FErrorMessage;
        LagInMonthsCbx.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.ValidatePointsCount(AFeature: IIFRFeature);
const OPNAME = 'TIFRFeatureValidator.ValidatePointsCount';
begin
  try
    with IFRFeatureDialog do
    begin
      FErrorMessage := '';
      if (NOT AFeature.Validate(FErrorMessage, 'IFRPointsCount')) then
        FAllErrorMessages.Add(Trim(FErrorMessage));
        NrOfPointsEdit.ContextValidationError :=  FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRFeatureValidator.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TIFRFeatureValidator.ProcessParameterChangeEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFeature       : IIFRFeature;
  lFieldIndex    : string;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with IFRFeatureDialog do
        begin
          if (FActiveControl = LagInMonthsCbx) then
          begin
            lFieldIndex := '';
            lFieldProperty := LagInMonthsCbx.FieldProperty;
            if (lFieldProperty <> nil) then
            begin
              lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              FAppModules.Changes.ShowParameterChanges
                (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              RePopulateDataViewer;
              FAppModules.Changes.SetParameterChanges(TRUE);
              Result := TRUE;
            end;
          end
          else
          if (FActiveControl = ValuesGrid) then
          begin
            lFieldIndex    := IntToStr(ValuesGrid.Row) + ',' + IntToStr((ValuesGrid.Col div 2) + 1);
            lFieldProperty := ValuesGrid.FieldProperty(ValuesGrid.Col);
            if (lFieldProperty <> nil) then
            begin
              lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              FAppModules.Changes.ShowParameterChanges
                (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              RePopulateDataViewer;
              FAppModules.Changes.SetParameterChanges(TRUE);
              Result := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.ProcessMetaDataEvent: boolean;
const OPNAME = 'TIFRFeatureValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  LFieldIndex    : string;
  lFeature       : IIFRFeature;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FFeatureID <> 0)) then
    begin
      lFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
      if (lFeature <> nil) then
      begin
        with IFRFeatureDialog do
        begin
          LFieldIndex    := '';
          lFieldProperty := nil;
          if (FActiveControl = FeatureNameEdit) then
            lFieldProperty := FeatureNameEdit.FieldProperty
          else
          if (FActiveControl = LagInMonthsCbx) then
            lFieldProperty := LagInMonthsCbx.FieldProperty
          else
          if (FActiveControl = NrOfPointsEdit) then
            lFieldProperty := NrOfPointsEdit.FieldProperty
          else
          if (FActiveControl = ExceedenceGrid) then
          begin
            LFieldIndex := IntToStr(ExceedenceGrid.Row+1);
            lFieldProperty := ExceedenceGrid.FieldProperty(1);
          end
          else
          if (FActiveControl = ValuesGrid) then
          begin
            lFieldIndex    := IntToStr(ValuesGrid.Row) + ',' + IntToStr((ValuesGrid.Col div 2) + 1);
            lFieldProperty := ValuesGrid.FieldProperty(ValuesGrid.Col);
          end;
          if (lFieldProperty <> nil) then
          begin
            lKeyValues := lFeature.GetKeyValues(lFieldProperty.FieldName, LFieldIndex);
            FAppModules.MetaData.ShowMetaData
              (lFieldProperty.FieldName, lKeyValues, LFieldIndex);
            RePopulateDataViewer;
            Result := TRUE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.DoOnGraphMonthCbxSelect(Sender : TObject);
const OPNAME = 'TIFRFeatureValidator.DoOnGraphMonthCbxSelect';
var
  LIFRFeature : TIFRFeature;
  LRowIndex,
  LRowCount: integer;
  LXValues : array[1..12] of double;
  LXValue,
  LYValue      : double;
  LTitle       : string;
  LMonth : integer;
  LIndex : integer;
  LCount : integer;
  LSystemConfig : IRunConfigurationData;

begin
  try
    IFRFeatureDialog.ClearChart;
    IFRFeatureDialog.PrepareChart;
    IFRFeatureDialog.MonthlyIFRGraph.Title.Text.Clear;
    LSystemConfig := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    for LRowIndex := Low(IFRFeatureDialog.InflowLineSeriesArray) to  High(IFRFeatureDialog.InflowLineSeriesArray) do
    begin
      IFRFeatureDialog.InflowLineSeriesArray[LRowIndex].Clear;
      IFRFeatureDialog.InflowLineSeriesArray[LRowIndex].Visible := False;
    end;
    for LRowIndex := Low(IFRFeatureDialog.IFRLineSeriesArray) to  High(IFRFeatureDialog.IFRLineSeriesArray) do
    begin
      IFRFeatureDialog.IFRLineSeriesArray[LRowIndex].Clear;
      IFRFeatureDialog.IFRLineSeriesArray[LRowIndex].Visible := False;
    end;
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByID(FFeatureID);

    if(LIFRFeature <> nil) then
    begin
      IFRFeatureDialog.MonthlyIFRGraph.Visible := True;
      IFRFeatureDialog.PrepareChart;
      IFRFeatureDialog.MonthlyIFRGraph.BottomAxis.Title.Caption :=
                           FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
      LRowCount := LIFRFeature.NrOfInflowIFRPoints;
      LMonth := IFRFeatureDialog.GraphMonthCbx.ItemIndex;
      if (LRowCount = 0) then Exit;

      for LRowIndex := 1 to LRowCount do
        LXValues[LRowIndex] := StrToFloat(FormatFloat('00000000000000.000',LIFRFeature.ExceedencePercentageByIndex[LRowIndex]));


      if LMonth = 0 then
      begin

        IFRFeatureDialog.MonthlyIFRGraph.Legend.Visible    := True;
        IFRFeatureDialog.MonthlyIFRGraph.Legend.Alignment   := laBottom;
       for LIndex := 1 to 12 do
        begin
          IFRFeatureDialog.DefinedLineSeriesArray[LIndex].Visible         := True;
          IFRFeatureDialog.DefinedLineSeriesArray[LIndex].Visible        := True;
          IFRFeatureDialog.DefinedLineSeriesArray[LIndex].SeriesColor     := C_Colors[LIndex];
          IFRFeatureDialog.DefinedLineSeriesArray[LIndex].SeriesColor    := C_Colors[LIndex];
          IFRFeatureDialog.MonthlyIFRGraph.Series[LIndex-1].ShowInLegend := True;
          IFRFeatureDialog.MonthlyIFRGraph.Series[LIndex-1].Color        := C_Colors[LIndex];
          IFRFeatureDialog.MonthlyIFRGraph.Series[LIndex-1].Title        := LSystemConfig.MonthNameByIndex[LIndex];

          for LCount := 12 downto 1 do
          begin

            LYValue :=  StrToFloat(FormatFloat('00000000000000.000',LIFRFeature.ReleaseByIndexAndMonth[LCount,LIndex]));
            if (LYValue <> NullFloat) then
            begin

              if(LXValues[LCount] = NullFloat) then
                LXValue := GetValuePercentile(LIFRFeature, LIFRFeature.InflowByIndexAndMonth[LCount,LIndex],LIndex)
              else
                LXValue := LXValues[LCount];

              if(LXValue = NullFloat) then Continue;

              LYValue := ConvertToMcM(LYValue,LCount);
              IFRFeatureDialog.DefinedLineSeriesArray[LIndex].AddXY(LXValue,LYValue);
            end;
          end;
        end;
      end
      else
      if (LMonth > 0) then
      begin
        IFRFeatureDialog.DefinedLineSeriesArray[LMonth].Visible         := True;
        IFRFeatureDialog.DefinedLineSeriesArray[LMonth].Visible        := True;
        IFRFeatureDialog.DefinedLineSeriesArray[LMonth].SeriesColor     := C_Colors[LMonth];
        IFRFeatureDialog.DefinedLineSeriesArray[LMonth].SeriesColor    := C_Colors[LMonth];
        IFRFeatureDialog.MonthlyIFRGraph.Legend.Visible := False;


        for LCount := 12 downto 1 do
        begin

          LYValue := StrToFloat(FormatFloat('00000000000000.000',LIFRFeature.ReleaseByIndexAndMonth[LCount,LMonth]));
          if(LYValue <> NullFloat) then
          begin
            if(LXValues[LCount] = NullFloat) then
              LXValue := GetValuePercentile(LIFRFeature, LIFRFeature.InflowByIndexAndMonth[LCount,LMonth],LMonth)
            else
              LXValue := LXValues[LCount];

            if(LXValue = NullFloat) then Continue;

            LYValue := ConvertToMcM(LYValue,LCount);
            IFRFeatureDialog.DefinedLineSeriesArray[LMonth].AddXY(LXValue,LYValue);
          end;
        end;
      end;


      IFRFeatureDialog.MonthlyIFRGraph.BottomAxis.Title.Caption := FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
      LTitle := FAppModules.Language.GetString('TIFRFeatureDialog.DefinedIFR');
      if(IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 0) then
        LTitle := LTitle + ' (m3/s)'
      else if(IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
        LTitle := LTitle + ' (Mm3/mon)';
      IFRFeatureDialog.MonthlyIFRGraph.LeftAxis.Title.Caption := LTitle;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRFeatureValidator.GetValuePercentile(AIFRFeature : TIFRFeature; const AYValue : double; const AMonth : integer): double;
const OPNAME = 'TIFRFeatureValidator.GetValuePercentile';
var
  LReferenceFlowData : TStringList;
  LIndex : integer;
  LPercentile: double;
begin
  Result := NullFloat;
  try
    if (AIFRFeature <> nil) then
    begin
      LReferenceFlowData := TStringList.Create;
      try
        if not AIFRFeature.GetNodeReferenceFlowData(LReferenceFlowData) then Exit;
        GetSortedDataByMonth(LReferenceFlowData,AMonth);

        for LIndex := 0 to LReferenceFlowData.Count-1 do
        begin
          LPercentile := Trunc((LIndex/LReferenceFlowData.Count)*100);
          if (StrToFloat(FormatFloat('00000000000000.000',
          StrToFloat(LReferenceFlowData[LIndex]))) =
            StrToFloat(FormatFloat('00000000000000.000',AYValue))) then
          begin
            Result := LPercentile;
            Exit;
          end;
        end;
        if (Result = NullFloat) and (AYValue = 999) then
          Result := 0;
      finally
        LReferenceFlowData.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TIFRFeatureValidator.GetChartLegend(AChart: TAbstractChart);
const OPNAME = 'TIFRFeatureValidator.GetChartLegend';
var
  LIndex : integer;
  LSystemConfig : IRunConfigurationData;
  LCount : integer;
begin
  try
    LSystemConfig := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData;
    LCount := 0;
    for LIndex := 0 to AChart.SeriesCount-1 do
    begin
       if (AChart.Series[LIndex] is TLineSeries) then
       begin
         if LCount <= 11 then
         begin
           AChart.Series[LIndex].ShowInLegend := True;
           AChart.Series[LIndex].Color        := C_Colors[LCount];
           AChart.Series[LIndex].Title        := LSystemConfig.MonthNameByIndex[LCount+1];
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


procedure TIFRFeatureValidator.DoOnInflowIFRMonthCbxSelect(Sender: TObject);
const OPNAME ='TIFRFeatureValidator.DoOnInflowIFRMonthCbxSelect';
var
  LIFRFeature   : TIFRFeature;
  LXValues,
  LYValues      : double;
  LCount,
  LDisplayMonth : integer;
  LInflowData,
  LReleaseData  : TStringList;
  LDaysInMonth  : double;
  LTitle        : string;
  LMaxFactor : double;
  LMinFactor : double;
begin
  try
    IFRFeatureDialog.InflowVsIFRChart.Title.Text.Clear;
    IFRFeatureDialog.InflowLineSeries.Clear;
    LDisplayMonth := IFRFeatureDialog.InflowIFRMonthCbx.ItemIndex;
    //IFRFeatureDialog.InflowVsIFRChart.Legend.Visible := True;
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                    CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByID(FFeatureID);

    LTitle := FAppModules.Language.GetString('TIFRFeatureDialog.DefinedIFR');
    if(IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 0) then
      LTitle := LTitle + ' (m3/s)'
    else if(IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
      LTitle := LTitle + ' (Mm3/mon)';
    IFRFeatureDialog.InflowVsIFRChart.LeftAxis.Title.Caption    :=  LTitle;

    LTitle := FAppModules.Language.GetString('TIFRFeatureDialog.DefinedReferenceFlow');
    if(IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 0) then
      LTitle := LTitle + ' (m3/s)'
    else if(IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
      LTitle := LTitle + ' (Mm3/mon)';
    IFRFeatureDialog.InflowVsIFRChart.BottomAxis.Title.Caption  := LTitle;

    LInflowData   := TStringList.Create;
    LReleaseData  := TStringList.Create;
    try
      if LIFRFeature = nil then
        Exit;
      //IFRFeatureDialog.InflowVsIFRChart.BottomAxis.SetMinMax(0,999);
      IFRFeatureDialog.InflowVsIFRChart.BottomAxis.Increment := 1;
      IFRFeatureDialog.InflowVsIFRChart.LeftAxis.AutomaticMaximum := True;
      IFRFeatureDialog.InflowVsIFRChart.BottomAxis.AutomaticMaximum := True;
      IFRFeatureDialog.InflowVsIFRChart.Title.Text.Text := IFRFeatureDialog.InflowIFRMonthCbx.Text;
      if (LDisplayMonth >= 0) then
      begin
        for LCount := 1 to 12 do
        begin
          LDaysInMonth := TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthDaysByIndex[LCount];
          LXValues := LIFRFeature.InflowByIndexAndMonth[LCount,LDisplayMonth+1];
          LYValues := LIFRFeature.ReleaseByIndexAndMonth[LCount,LDisplayMonth+1];
          if LXValues = 999.999 then
            Continue;
          if(LXValues <> NullFloat) AND (LYValues <> NullFloat) then
          begin
            if(IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
              LYValues := (LYValues*(60*60*24*LDaysInMonth))/ Power(10, 6);
            IFRFeatureDialog.InflowLineSeries.AddXY(LXValues,LYValues);
          end;
        end;
      end;
      LMaxFactor := IFRFeatureDialog.InflowLineSeries.YValues.MaxValue;
      LMinFactor := IFRFeatureDialog.InflowLineSeries.YValues.MinValue;
      LMaxFactor := LMaxFactor * 0.05;
      LMinFactor := LMinFactor * 0.05;
      IFRFeatureDialog.InflowVsIFRChart.LeftAxis.SetMinMax
       (IFRFeatureDialog.InflowLineSeries.YValues.MinValue - LMinFactor,
        IFRFeatureDialog.InflowLineSeries.YValues.MaxValue + LMaxFactor);

    finally
      LInflowData.Free;
      LReleaseData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.DoOnRefDemandCbxSelect(Sender : TObject);
const OPNAME ='TIFRFeatureValidator.DoOnRefDemandCbxSelect';
var
  LIFRFeature : TIFRFeature;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByID(FFeatureID);
    if (LIFRFeature <> nil) and (IFRFeatureDialog.RefMonthsCbx.Text <> '') then
      {case IFRFeatureDialog.RefDemandCbx.ItemIndex of
        0 : PopulateReferenceFlow(LIFRFeature);
        1 : PopulateRequirementAndFlow(LIFRFeature);
      end;
      }
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.DoRefMonthsCbxSelect(Sender : TObject);
const OPNAME ='TIFRFeatureValidator.DoRefMonthsCbxSelect';
var
  LIFRFeature : TIFRFeature;
  LIndex : integer;
  LMonth : integer;
  LExcedence : array[1..12] of double;
  LRowIndex : integer;
  LRowCount : integer;
  //LDaysInMonth  : double;
  LYValue, LXValue : double;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                   CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByID(FFeatureID);
    if (LIFRFeature <> nil) {and (IFRFeatureDialog.RefDemandCbx.Text <> '')} then
    begin


      LRowCount := LIFRFeature.NrOfInflowIFRPoints;
      LMonth := IFRFeatureDialog.RefMonthsCbx.ItemIndex+1;


      for LRowIndex := 1 to LRowCount do
        LExcedence[LRowIndex] := StrToFloat(FormatFloat('00000000000000.000',LIFRFeature.ExceedencePercentageByIndex[LRowIndex]));

      {for LRowIndex := 1 to LRowCount do
      begin

        LExcedence[LRowIndex] := StrToFloat(FormatFloat('00000000000000.000',LIFRFeature.ExceedencePercentageByIndex[LRowIndex]));

        if(LExcedence[LRowIndex] = NullFloat) then
          LExcedence[LRowIndex] := Trunc(LRowIndex/LRowCount*100);
      end;
      }

      IFRFeatureDialog.ReferenceFlowVsDemand.Legend.Visible := True;
      IFRFeatureDialog.ReferenceFlowVsDemand.Visible := True;
      IFRFeatureDialog.RefLineSeries.Clear;
      IFRFeatureDialog.DemLineSeries.Clear;
      IFRFeatureDialog.ReferenceFlowVsDemand.Series[0].Title := 'Reference Flow';
      IFRFeatureDialog.ReferenceFlowVsDemand.Series[1].Title := 'IFR';
      //IFRFeatureDialog.ReferenceFlowVsDemand.BottomAxis.SetMinMax(0,100);
      //IFRFeatureDialog.ReferenceFlowVsDemand.BottomAxis.Increment := 10;
      //IFRFeatureDialog.ReferenceFlowVsDemand.LeftAxis.SetMinMax(0,20);
       IFRFeatureDialog.ReferenceFlowVsDemand.BottomAxis.Title.Caption :=
                           FAppModules.Language.GetString('TIFRFeatureDialog.Exceedenceprobability');
      //LRowCount := LIFRFeature.NrOfInflowIFRPoints;
      //LRowIndex := 0;
      for LIndex := 12 downto 1 do
      begin
        if (LIFRFeature.InflowByIndexAndMonth[LIndex,LMonth] <> NullFloat) then
        begin
          if LIFRFeature.InflowByIndexAndMonth[LIndex,LMonth] >= 999.999 then
            Continue;
          LYValue := ConvertToMcM(LIFRFeature.InflowByIndexAndMonth[LIndex,LMonth],LMonth);
          IFRFeatureDialog.RefLineSeries.AddY(((LYValue)));
        end;
        if (LIFRFeature.InflowByIndexAndMonth[LIndex,LMonth] <> NullFloat) and
           (LIFRFeature.ReleaseByIndexAndMonth[LIndex,LMonth] <> NullFloat)then
        begin

          //LRowIndex := LRowIndex+1;
          if(LExcedence[LIndex] = NullFloat) then
            LExcedence[LIndex] := GetValuePercentile(LIFRFeature,LIFRFeature.InflowByIndexAndMonth[LIndex,LMonth],LMonth);
          if(LExcedence[LIndex] = NullFloat) then
            Continue;

          LXValue := ConvertToMcM(LIFRFeature.ReleaseByIndexAndMonth[LIndex,LMonth],LMonth);
          IFRFeatureDialog.DemLineSeries.AddY(LXValue,FloatToStr(LExcedence[LIndex]));

        end;
      end;
    end;

    {  case IFRFeatureDialog.RefDemandCbx.ItemIndex of
        0 : PopulateReferenceFlow(LIFRFeature);
        1 : PopulateRequirementAndFlow(LIFRFeature);
      end;
    }
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TIFRFeatureValidator.PopulateReferenceFlow(AIFRFeature: TIFRFeature);
const OPNAME = 'TIFRFeatureValidator.PopulateReferenceFlow';
var
  LStringValue: string;
  LFloatValue : double;
  LReferenceFlowData,
  LMonthData,
  LLineData   : TStringList;
  LYValue,
  LPercentile: double;
  LIndex : integer;
  LDisplayMonth : integer;
begin
  try
    LDisplayMonth := IFRFeatureDialog.RefMonthsCbx.ItemIndex;
    IFRFeatureDialog.ReferenceFlowVsDemand.Legend.Visible := False;
    IFRFeatureDialog.ReferenceFlowVsDemand.Visible := True;
    IFRFeatureDialog.RefLineSeries.Clear;
    IFRFeatureDialog.DemLineSeries.Clear;

    IFRFeatureDialog.ReferenceFlowVsDemand.BottomAxis.Title.Caption :=
                   FAppModules.Language.GetString('TIFRSiteDialog.ExceedenceProbability');
    if (IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 1) then
      IFRFeatureDialog.ReferenceFlowVsDemand.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedReferenceFlow'),
                           [FAppModules.Language.GetString('MasterControl.MillionM3perAnnum')])
    else
    if (IFRFeatureDialog.UnitsOptionRadioGroup.ItemIndex = 0) then
      IFRFeatureDialog.ReferenceFlowVsDemand.LeftAxis.Title.Caption   :=
                           Format(FAppModules.Language.GetString('TIFRChannelComplianceGraph.SimulatedReferenceFlow'),
                           [FAppModules.Language.GetString('MasterControl.M3perSecond')]);
    IFRFeatureDialog.ReferenceFlowVsDemand.Title.Text.Text := IFRFeatureDialog.RefMonthsCbx.Text;
    if(AIFRFeature = nil) then
      Exit;
    if (LDisplayMonth = 0) then
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
        LStringValue    := LLineData[LDisplayMonth];
        LFloatValue     := StrToFloat(LStringValue);
        LMonthData.Add(FormatFloat('00000000000000.000',LFloatValue));
      end;

      LLineData.Clear;
      for LIndex := LMonthData.Count-1 downto 0 do
        LLineData.Add(LMonthData[LIndex]);

      IFRFeatureDialog.ReferenceFlowVsDemand.BottomAxis.SetMinMax(0,100);
      IFRFeatureDialog.ReferenceFlowVsDemand.BottomAxis.Increment := 10;
      IFRFeatureDialog.RefLineSeries.Visible := True;

      for LIndex := 0 to LLineData.Count-1 do
      begin
        LPercentile := (LIndex/LLineData.Count)*100;
        LYValue := StrToFloat(LLineData[LIndex]);
        LYValue := ConvertToMcM(LYValue,LDisplayMonth);
        IFRFeatureDialog.RefLineSeries.AddXY(LPercentile,LYValue);
      end;
    finally
      LMonthData.Free;
      LLineData.Free;
      LReferenceFlowData.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TIFRFeatureValidator.UnitOptionsRadioGroupOnClick(Sender: TObject);
const OPNAME ='TIFRFeatureValidator.UnitOptionsRadioGroupOnClick';
var
  LGraphMonthCbxIndex : integer;
  LInflowIFRMonthCbx  : integer;
  LRefFlowMonthcbx    : integer;
begin
  try
    LGraphMonthCbxIndex  := IFRFeatureDialog.GraphMonthCbx.ItemIndex;
    LInflowIFRMonthCbx   := IFRFeatureDialog.InflowIFRMonthCbx.ItemIndex;
    LRefFlowMonthcbx     := IFRFeatureDialog.RefMonthsCbx.ItemIndex;
    RePopulateDataViewer;
    IFRFeatureDialog.GraphMonthCbx.ItemIndex    := LGraphMonthCbxIndex;
    IFRFeatureDialog.InflowIFRMonthCbx.ItemIndex := LInflowIFRMonthCbx;
    IFRFeatureDialog.RefMonthsCbx.ItemIndex :=  LRefFlowMonthcbx;
    DoOnGraphMonthCbxSelect(nil);
    DoOnInflowIFRMonthCbxSelect(nil);
    DoRefMonthsCbxSelect(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.rdgAnnualMonthlyOptionOnClick(Sender: TObject);
const OPNAME = 'TIFRFeatureValidator.rdgAnnualMonthlyOptionOnClick';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRFeatureValidator.UpdateSiteData;
const OPNAME ='TIFRFeatureValidator.UpdateSiteData';
var
  LIFRSite       : TIFRSiteDataObject;
  LIFRSiteList   : TIFRSiteDataList;
  LIFRFeature    : TIFRFeature;
  LIdentifier    : integer;
  LIndex         : integer;
begin
  try
    with IFRFeatureDialog do
    begin
      if (SitesCbx.ItemIndex < 0) then Exit;
      LIndex := IFRFeatureDialog.SitesCbx.ItemIndex;
      LIdentifier := Integer(SitesCbx.Items.Objects[LIndex]);

      LIFRSiteList := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.IFRSiteList;
      LIFRSite := LIFRSiteList.IFRSiteDataByIdentifier[LIdentifier];
      if(LIFRSite <> nil) then
      begin
        LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIFRFeatureList.CastMonthlyIFRFeatureByID(FFeatureID);
        if(LIFRFeature <> nil) then
        begin
          LIFRFeature.PopulateWithSiteData(LIFRSite);
          RePopulateDataViewer;
          DoContextValidation(dvtIFRFeature);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TIFRFeatureValidator.UpdateCalculationOption;
const OPNAME = 'TIFRFeatureValidator.UpdateCalculationOption';
var
  lIFRFeature : IIFRFeature;
  lMessage    : string;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.IFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CalcOptionCbx.FieldProperty.FieldName,
            CalcOptionCbx.Text, lMessage)) then
        begin
          CalcOptionCbx.ValidationError := lMessage;
          lIFRFeature.CalculationOption := StrToFloat(Trim(CalcOptionCbx.Text));
          CalcOptionCbx.ItemIndex  := CalcOptionCbx.Items.IndexOf(FloatToStr(lIFRFeature.CalculationOption));
          ResizeGrids;
          RePopulateGrids;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
procedure TIFRFeatureValidator.GetSortedData(AData: TStrings);
const OPNAME = 'TIFRFeatureValidator.GetSortedData';
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

procedure TIFRFeatureValidator.OnUpdateIFRFromReferenceInflowsClicked(Sender: TObject);
const OPNAME ='TIFRFeatureValidator.OnUpdateIFRFromReferenceInflowsClicked';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkFeaturesData.CastIFRFeatureList.UpdateIFRFromReferenceInflows :=
      IFRFeatureDialog.chkboxUpdateIFRFromReferenceInflows.Checked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.OnReferenceNodesSelectionChange(Sender: TObject);
const OPNAME ='TIFRFeatureValidator.OnReferenceNodesSelectionChange';
var NodeIndex : Integer;
    TopIndex : Integer;
begin
  try
     if(IFRFeatureDialog.ReferenceNodesCheckLbx.HasValueChanged) then
     Begin
       NodeIndex := ReferenceNodesSelectionSave;
       TopIndex := IFRFeatureDialog.ReferenceNodesCheckLbx.TopIndex;
       UpdateReferenceNodes;
       IFRFeatureDialog.ReferenceNodesCheckLbx.selected[NodeIndex] := True;
       IFRFeatureDialog.ReferenceNodesCheckLbx.TopIndex := TopIndex;
       CalculteMAROfSelectedNodes;
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.OnAfterPasteExceedenceGridColumnData(ASender: TObject);
const OPNAME ='TIFRFeatureValidator.OnAfterPasteExceedenceGridColumnData';
var
  LIFRFeature : IIFRFeature;
  LValue      : Double;
  LIndex      : Integer;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                   IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if(LIFRFeature <> nil) then
    begin
      if(ASender = IFRFeatureDialog.ExceedenceGrid) then
      begin
        for LIndex := 1 to TFieldStringGrid(ASender).RowCount do
        begin
          LValue := StrToFloat(TFieldStringGrid(ASender).Cells[1,LIndex - 1]);
          LIFRFeature.ExceedencePercentageByIndex[LIndex] := LValue;
        end;
        RePopulateDataViewer;
        DoContextValidation(dvtIFRFeature);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.OnAfterPasteValuesGridColumnData(ASender: TObject);
const OPNAME ='TIFRFeatureValidator.OnAfterPasteValuesGridColumnData';
var
  LIFRFeature : IIFRFeature;
  LValue      : Double;
  LIndex      : Integer;
  LCol        : Integer;
  LMonth      : Integer;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                   .IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if(LIFRFeature <> nil) then
    begin
      if(ASender = IFRFeatureDialog.ValuesGrid) then
      begin
        LCol   := IFRFeatureDialog.ValuesGrid.Col;
        LMonth := (LCol div 2) + 1;
        if(LCol mod 2 = 0) then
        begin
          for LIndex := 1 to IFRFeatureDialog.ValuesGrid.RowCount - 1 do
          begin
            LValue := StrToFloat(TFieldStringGrid(ASender).Cells[LCol,LIndex]);
            LIFRFeature.InflowByIndexAndMonth[LIndex, LMonth] := LValue;
          end;
        end
        else
        begin
          for LIndex := 1 to IFRFeatureDialog.ValuesGrid.RowCount - 1 do
          begin
            LValue := StrToFloat(TFieldStringGrid(ASender).Cells[LCol,LIndex]);
            LIFRFeature.ReleaseByIndexAndMonth[LIndex, LMonth] := LValue;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.OnAfterPasteValuesGridData(ASender: TObject);
const OPNAME ='TIFRFeatureValidator.OnAfterPasteValuesGridData';
var
  LIFRFeature : IIFRFeature;
  LValue      : Double;
  LRowIndex   : Integer;
  LColIndex   : Integer;
  LMonth      : Integer;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                   .IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if(LIFRFeature <> nil) then
    begin
      if(ASender = IFRFeatureDialog.ValuesGrid) then
      begin
        for LRowIndex := 1 to IFRFeatureDialog.ValuesGrid.RowCount - 1 do
        begin
          for LColIndex := 1 to IFRFeatureDialog.ValuesGrid.ColCount do
          begin
            LMonth := ((LColIndex - 1) div 2) + 1; 
            if((LColIndex - 1) mod 2 = 0) then
            begin
              LValue := StrToFloat(TFieldStringGrid(ASender).Cells[LColIndex - 1,LRowIndex]);
              LIFRFeature.InflowByIndexAndMonth[LRowIndex, LMonth] := LValue;
            end
            else
            begin
              LValue := StrToFloat(TFieldStringGrid(ASender).Cells[LColIndex - 1,LRowIndex]);
              LIFRFeature.ReleaseByIndexAndMonth[LRowIndex, LMonth] := LValue;
            end;
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.ReferenceNodesSelectionSave : integer;
const OPNAME ='TIFRFeatureValidator.ReferenceNodesSelectionSave';
var i : integer;
    LastSelectedReference : integer;
begin
result := -1;
for i := 0 to IFRFeatureDialog.ReferenceNodesCheckLbx.Items.Count -1 do
 begin
   if IFRFeatureDialog.ReferenceNodesCheckLbx.Selected[i] then
     Begin
       LastSelectedReference := i;
       result := LastSelectedReference;
     end;
 end;

end;

procedure TIFRFeatureValidator.UpdateMARDisplay(AReservoirName: string);
const OPNAME = 'TIFRFeatureValidator.UpdateMARDisplay';
var
  LReservoirData : IReservoirData;
  LReservoirDataList : IReservoirDatalist;
  LCatchmentRef : Integer;
  LParamSetup : TParamSetup;
  LParamReference : IParamReference;
  LFileNames : TStringList;
begin
  try
    LFileNames := TStringList.Create;
    try
      LReservoirDataList := TPlanningModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
      LReservoirData := LReservoirDataList.ReservoirOrNodeByName[AReservoirName];
      if (LReservoirData <> nil) then
      begin
        LCatchmentRef := LReservoirData.ReservoirConfigurationData.CatchmentRef;
        LParamSetup   := TParamSetup.Create(AppModules);
        LParamReference := LParamSetup.ReferenceDataByCatchNumber[LCatchmentRef];
        TYieldModelDataObject(FAppModules.Model.ModelData).GetHydrologyFilesForCatchment(LCatchmentRef,LFileNames);
        PopulateFileNames(LFileNames);
      end;
    finally
      LFileNames.Free;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateMonthlyIFRLossGrid(AIndex: integer; AValue: string);
const OPNAME = 'TIFRFeatureValidator.UpdateMonthlyIFRLossGrid';
var
  lIFRFeature : IIFRFeature;
  lValue      : string;
  lMessage    : string;
begin
  try
    lIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (lIFRFeature <> nil) then
    begin
      with IFRFeatureDialog do
      begin
        MonthlyIFRLossGrid.ValidationError[1, AIndex, gveCellField] := '';
        if (Trim(AValue) = '') then
          AValue := '0.0';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'MonthlyIFRLoss', AValue, lMessage, AIndex)) then
        begin
          lValue := FormatFloat('##0.00',StrToFloat(AValue));
          lIFRFeature.MonthlyIFRLossByIndex[AIndex] := StrToFloat(lValue);
          RePopulateDataViewer;
          DoContextValidation(dvtIFRFeature);
        end
        else
          MonthlyIFRLossGrid.ValidationError[1, AIndex-1, gveCellField] := lMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRFeatureValidator.CalculateMAR(AFileNames: TStringList):double;
const OPNAME = 'TIFRFeatureValidator.CalculateMAR';
var
  LFieldName,
  LFileExt,
  LFileName: string;
  LDataSet: TAbstractModelDataset;
  LCount: integer;
  LIndex: integer;
  LMonthValue: double;

  LAnnualTotalVal,
  LMARTotalVal,
  LMARTotalAve : double;
  LMARTotalYrs : integer;
begin
  Result := 0.0;
  try
    LMARTotalAve := 0.0;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      if (LDataSet <> nil) then
      begin
        for LCount := 0 to AFileNames.Count -1 do
        begin
          LFileName := AFileNames[LCount];
          LFileName := ExtractFileName(LFileName);
          LFileExt  := UpperCase(ExtractFileExt(LFileName));

          if (LFileExt <> '.INC') then
            Continue;

          LDataSet.DataSet.Close;
          if not TYieldModelDataObject(FAppModules.Model.ModelData).GetHydrologyFileDataSet(AFileNames[LCount],LDataSet) then
            Continue;
          if not LDataSet.DataSet.Active then
            Continue;
          if (LDataSet.DataSet.Eof and LDataSet.DataSet.Bof) then
            Continue;
          LMARTotalVal  := 0.0;
          LMARTotalYrs  := 0;
          while not LDataSet.DataSet.Eof do
          begin
            LAnnualTotalVal := 0;
            for LIndex := 0 to 11 do
            begin
              LFieldName  := Format('%s%2.2d',['HydroMonthValue',LIndex + 1]);
              LMonthValue := LDataSet.DataSet.FieldByName(LFieldName).AsFloat;
              LAnnualTotalVal := LAnnualTotalVal + LMonthValue;
            end;
            LMARTotalVal  := LMARTotalVal  + LAnnualTotalVal;
            LMARTotalYrs  := LMARTotalYrs + 1;
            LDataSet.DataSet.Next;
          end;
          LMARTotalAve := (LMARTotalVal / LMARTotalYrs);
        end;
        Result := LMARTotalAve;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;  

procedure TIFRFeatureValidator.PopulateFileNames(AFileNameContainer: TStringList);
const OPNAME = 'TIFRFeatureValidator.PopulateFileNames';
var
  LFileExt,
  LFileName: string;
  LCount: integer;
begin
  try
    FFileNameContainer.Clear;
    FFileNameContainer.Add('');
    if Assigned(AFileNameContainer) then
    begin
      for LCount := 0 to AFileNameContainer.Count - 1  do
      begin
        LFileName := AFileNameContainer[LCount];
        LFileName := ExtractFileName(LFileName);
        LFileExt := UpperCase(ExtractFileExt(LFileName));
        if LFileExt = '.INC' then
          FFileNameContainer.Strings[0] := LFileName;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.GetSelectedNodes(out ANodesNameList: TStringList);
const OPNAME = 'TIFRFeatureValidator.GetSelectedNodes';
var
  LIndex: Integer;
begin
  try
    for LIndex := 0 to IFRFeatureDialog.ReferenceNodesCheckLbx.Count -1 do
    begin
      if IFRFeatureDialog.ReferenceNodesCheckLbx.Checked[LIndex] then
      begin
        ANodesNameList.Add(
        ReturnSubstringFromChar(IFRFeatureDialog.ReferenceNodesCheckLbx.Items.Strings[LIndex],')'));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.CalculteMAROfSelectedNodes;
const OPNAME = 'TIFRFeatureValidator.CalculteMAROfSelectedNodes';
var
  LIndex,
  LCounter : Integer;
  LReservoirNameslist : TStringList;
  LMARValue: double;
begin
  try
    LMARValue := 0.0;
    LReservoirNameslist := TStringList.Create;
    try
      GetSelectedNodes(LReservoirNameslist);
      for LCounter := 0 to LReservoirNameslist.Count - 1 do
      begin
        UpdateMARDisplay(LReservoirNameslist.Strings[LCounter]);
        for LIndex := 0 to FFileNameContainer.Count - 1 do
        begin
          LMARValue := LMARValue + CalculateMAR(FFileNameContainer);
        end;
      end;
      IFRFeatureDialog.TotalMAREdit.Text := '';
      IFRFeatureDialog.TotalMAREdit.Text := Format('%.5f',[LMARValue]);
    finally
      LReservoirNameslist.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.OnIFRExistsClick(Sender: TObject);
const OPNAME = 'TIFRFeatureValidator.OnIFRExistsClick';
begin
  try
    if(IFRFeatureDialog.chkboxFIFRFeatureExists.HasValueChanged) then
      UpdateIFRFeatureExists;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateIFRFeatureExists;
const OPNAME = 'TIFRFeatureValidator.UpdateIFRFeatureExists';
var
  LIFRFeatureExists : integer;
  LErrorMessage: string;
  LIFRFeature  : IIFRFeature;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (LIFRFeature <> nil) then
    begin
      LIFRFeatureExists := LIFRFeature.IFRStatusIndicator;

      if((LIFRFeatureExists = 1) and ( not IFRFeatureDialog.ChkboxFIFRFeatureExists.Checked) or
         (LIFRFeatureExists <> 1) and (IFRFeatureDialog.ChkboxFIFRFeatureExists.Checked)) then
      begin
        if IFRFeatureDialog.chkboxFIFRFeatureExists.Checked then
          LIFRFeatureExists := 1
        else
          LIFRFeatureExists := 0;

      if FAppModules.FieldProperties.ValidateFieldProperty(
         IFRFeatureDialog.chkboxFIFRFeatureExists.FieldProperty.FieldName,
         IntToStr(LIFRFeatureExists),LErrorMessage) then
        begin
          LIFRFeature.IFRStatusIndicator := LIFRFeatureExists;
          IFRFeatureDialog.ChkboxFIFRFeatureExists.Checked := (LIFRFeature.IFRStatusIndicator = 1);
        end
        else
        begin
          IFRFeatureDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.OnIFRLossClick(Sender: TObject);
const OPNAME = 'TIFRFeatureValidator.OnIFRLossClick';
begin
  try
    if(IFRFeatureDialog.IFRLoss.HasValueChanged) then
      UpdateIFRLoss;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIFRFeatureValidator.UpdateIFRLoss;
const OPNAME = 'TIFRFeatureValidator.UpdateIFRLoss';
var
  LIFRLoss : integer;
  LErrorMessage: string;
  LIFRFeature  : IIFRFeature;
begin
  try
    LIFRFeature := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IFRFeatureList.MonthlyIFRFeatureByID[FFeatureID];
    if (LIFRFeature <> nil) then
    begin
      LIFRLoss := LIFRFeature.IFRLoss;

      if((LIFRLoss = 1) and ( not IFRFeatureDialog.IFRLoss.Checked) or
         (LIFRLoss <> 1) and (IFRFeatureDialog.IFRLoss.Checked)) then
      begin
        if IFRFeatureDialog.IFRLoss.Checked then
          LIFRLoss := 1
        else
          LIFRLoss := 0;

      if FAppModules.FieldProperties.ValidateFieldProperty(
         IFRFeatureDialog.IFRLoss.FieldProperty.FieldName,
         IntToStr(LIFRLoss),LErrorMessage) then
        begin
          LIFRFeature.IFRLoss := LIFRLoss;
          IFRFeatureDialog.IFRLoss.Checked := (LIFRFeature.IFRLoss = 1);
          RePopulateDataViewer;
          DoContextValidation(dvtIFRFeature);
        end
        else
        begin
          IFRFeatureDialog.ShowError(LErrorMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

