//
//
//  UNIT      : Contains TMonthlyReferenceFlowValidator Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 20/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//


unit UMonthlyReferenceFlowValidator;

interface
uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  VCLTee.Chart,
  VCLTee.Series,
  VCL.Graphics,
  VCL.Dialogs,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  Math,
  VCL.Forms,
  Windows,
  Contnrs,

  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UDataComponent,
  UDailyDiversionDataObject,
  UDailyDiversionGaugeData,
  UMonthlyReferenceFlowDialog,
  UDailyDiversionContextValidationType,
  UGenericModelLinkClasses;
type
  TActiveSubTab = (actDailyGrid,actDailyGraph,actMonthlyGrid,actMonthlyGraph);
  TMonthlyReferenceFlowValidator = class (TAbstractDataDialogValidator)
  protected
    FActiveSubTab              : TActiveSubTab;
    FDailyDataChanged,
    FDailyGraphDataChanged,
    FMonthlyGraphDataChanged,
    FMonthlyDataChanged        : boolean;
    FThresholdValuesHasChanged : boolean;
    procedure CreateMemberObjects; override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject;ACol: Integer; ARow: Integer); override;
    procedure DoAddAddDailyDataClick(Sender: TObject);
    procedure DoDeleteDailyDataClick(Sender: TObject);
    procedure DorgbDailyDataQualityClick(Sender: TObject);
    procedure OnstrgrdDailyFlowSelectCell(Sender : TObject;ACol,ARow: Integer;var CanSelect : Boolean);
    procedure OnstrgrdDailyFlowTopLeftChanged (Sender : TObject);
    procedure OnTabChanged(Sender: Tobject);

    procedure PopulateDailygraph(ADiversionGauge : TDiversionGauge);
    procedure PopulateDailyGrid(ADiversionGauge : TDiversionGauge);
    procedure PopulateMonthlyGridData(ADiversionGauge : TDiversionGauge);
    procedure PopulateMonthlyGraphData(ADiversionGauge : TDiversionGauge);
    procedure RepopulateDataViewer;
    function GetMonthlyReferenceFlowDialog : TMonthlyReferenceFlowDialog;
    procedure UpdateDailyDataGrid;
    procedure UpdatePlace(ADiversionGauge : TDiversionGauge);
    procedure UpdateLatitude(ADiversionGauge : TDiversionGauge);
    procedure UpdateLongitude(ADiversionGauge : TDiversionGauge);
    procedure UpdateCatchmentArea(ADiversionGauge : TDiversionGauge);
    procedure UpdateCatchmentFactor(ADiversionGauge : TDiversionGauge);
    procedure UpdateDailyMonthlyThresholdValues;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation(AValidationType: TDialogValidationType);

    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;

    property MonthlyReferenceFlowDialog : TMonthlyReferenceFlowDialog read GetMonthlyReferenceFlowDialog;
    property ActiveSubTab               : TActiveSubTab               read FActiveSubTab                  write FActiveSubTab;
end;
implementation
uses
  System.UITypes,
  SysUtils,
  UConstants,
  UDataEditComponent,
  UDailyDivesionProgressBar,
  UErrorHandlingOperations, VCL.Grids;
{ DailyDiversionValidator }


procedure TMonthlyReferenceFlowValidator.OnTabChanged(Sender: Tobject);
const OPNAME = 'TMonthlyReferenceFlowValidator.OnTabChanged';
begin
  try
    if MonthlyReferenceFlowDialog.pgcAvgFlowFactor.ActivePage =
      MonthlyReferenceFlowDialog.tbsDailyFlowGraph  then
      ActiveSubTab := actDailyGraph;

    if MonthlyReferenceFlowDialog.pgcAvgFlowFactor.ActivePage =
      MonthlyReferenceFlowDialog.tbsDailyFlowGrid  then
      ActiveSubTab := actDailyGrid;
    if MonthlyReferenceFlowDialog.pgcAvgFlowFactor.ActivePage =
      MonthlyReferenceFlowDialog.tbsMonthlyFlowGrid  then
      ActiveSubTab := actMonthlyGrid;
    if MonthlyReferenceFlowDialog.pgcAvgFlowFactor.ActivePage =
      MonthlyReferenceFlowDialog.tbsMonthlyFlowGraph  then
      ActiveSubTab := actMonthlyGraph;

    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyReferenceFlowValidator.ClearDataViewer;
const OPNAME = 'TMonthlyReferenceFlowValidator.ClearDataViewer';
var
  LRow    : integer;
  LCol    : integer;
begin
  inherited ClearDataViewer;
  try
    MonthlyReferenceFlowDialog.edtStationNo.Text := '';
    MonthlyReferenceFlowDialog.edtPlace.Text := '';
    MonthlyReferenceFlowDialog.edtLatidute.Text := '';
    MonthlyReferenceFlowDialog.edtLongitude.Text := '';
    MonthlyReferenceFlowDialog.edtCatchmentArea.Text := '';
    if FDailyDataChanged then
    begin
      for LCol := 0 to MonthlyReferenceFlowDialog.strgrdDailyFlow.ColCount -1 do
      begin
        for LRow := 1 to MonthlyReferenceFlowDialog.strgrdDailyFlow.RowCount -1 do
          MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[LCol,LRow] := '';
      end;

      MonthlyReferenceFlowDialog.strgrdDailyFlow.ColCount         := 4;
      MonthlyReferenceFlowDialog.strgrdDailyFlow.RowCount         := 2;
    end;
    if FMonthlyDataChanged then
    begin
      for LCol := 0 to MonthlyReferenceFlowDialog.strgrdMonthlyFlow.ColCount -1 do
      begin
        for LRow := 1 to MonthlyReferenceFlowDialog.strgrdMonthlyFlow.RowCount -1 do
          MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[LCol,LRow] := '';
      end;
      MonthlyReferenceFlowDialog.strgrdMonthlyFlow.ColCount         := 14;
      MonthlyReferenceFlowDialog.strgrdMonthlyFlow.RowCount         := 2;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyReferenceFlowValidator.CreateMemberObjects;
const OPNAME = 'TMonthlyReferenceFlowValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TMonthlyReferenceFlowDialog.Create(nil, FAppModules);
    MonthlyReferenceFlowDialog.edtStationNo.FieldProperty := FAppModules.FieldProperties.FieldProperty('StationNo');

    MonthlyReferenceFlowDialog.edtPlace.FieldProperty := FAppModules.FieldProperties.FieldProperty('Place');
    MonthlyReferenceFlowDialog.edtPlace.OnEnter       := OnEditControlEnter;
    MonthlyReferenceFlowDialog.edtPlace.OnExit        := OnEditControltExit;
    MonthlyReferenceFlowDialog.edtPlace.IsEnabled     := True;

    MonthlyReferenceFlowDialog.edtLatidute.FieldProperty := FAppModules.FieldProperties.FieldProperty('Latitude');
    MonthlyReferenceFlowDialog.edtLatidute.OnEnter       := OnEditControlEnter;
    MonthlyReferenceFlowDialog.edtLatidute.OnExit        := OnEditControltExit;
    MonthlyReferenceFlowDialog.edtLatidute.IsEnabled     := True;

    MonthlyReferenceFlowDialog.edtLongitude.FieldProperty := FAppModules.FieldProperties.FieldProperty('Longitude');
    MonthlyReferenceFlowDialog.edtLongitude.OnEnter       := OnEditControlEnter;
    MonthlyReferenceFlowDialog.edtLongitude.OnExit        := OnEditControltExit;
    MonthlyReferenceFlowDialog.edtLongitude.IsEnabled     := True;

    MonthlyReferenceFlowDialog.edtCatchmentArea.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentArea');
    MonthlyReferenceFlowDialog.edtCatchmentArea.OnEnter       := OnEditControlEnter;
    MonthlyReferenceFlowDialog.edtCatchmentArea.OnExit        := OnEditControltExit;
    MonthlyReferenceFlowDialog.edtCatchmentArea.IsEnabled     := True;

    MonthlyReferenceFlowDialog.edtCatchmentFactor.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentScaleFactor');
    MonthlyReferenceFlowDialog.edtCatchmentFactor.OnEnter       := OnEditControlEnter;
    MonthlyReferenceFlowDialog.edtCatchmentFactor.OnExit        := OnEditControltExit;
    MonthlyReferenceFlowDialog.edtCatchmentFactor.IsEnabled     := True;

    MonthlyReferenceFlowDialog.BtnAddDailyData.OnClick        := DoAddAddDailyDataClick;
    MonthlyReferenceFlowDialog.BtnDeleteDailyData.OnClick     := DoDeleteDailyDataClick;
    MonthlyReferenceFlowDialog.strgrdDailyFlow.OnBeforeCellChange := OnStringGridCellDataHasChanged;

    MonthlyReferenceFlowDialog.strgrdDailyFlow.OnSelectCell := OnstrgrdDailyFlowSelectCell;
    MonthlyReferenceFlowDialog.strgrdDailyFlow.OnTopLeftChanged := OnstrgrdDailyFlowTopLeftChanged;

    MonthlyReferenceFlowDialog.dtpDiversionDate.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentArea');
    MonthlyReferenceFlowDialog.dtpDiversionDate.OnEnter       := OnEditControlEnter;
    MonthlyReferenceFlowDialog.dtpDiversionDate.OnExit        := OnEditControltExit;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMonthlyReferenceFlowValidator.DoContextValidation';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TMonthlyReferenceFlowValidator.Initialise: boolean;
const OPNAME = 'TMonthlyReferenceFlowValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ClearDataViewer;
    FDailyDataChanged := True;
    FDailyGraphDataChanged := True;
    FMonthlyGraphDataChanged := True;
    FMonthlyDataChanged := True;

    MonthlyReferenceFlowDialog.rgbDailyDataQuality.OnClick    := DorgbDailyDataQualityClick;
    MonthlyReferenceFlowDialog.pgcAvgFlowFactor.OnChange      := OnTabChanged;
    //MonthlyReferenceFlowDialog.rgbThreshold.OnClick           := DorgbThresholdClick;
    //MonthlyReferenceFlowDialog.edtThreshold.OnKeyPress         := DoedtThresholdOnKeyPress;
    MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.OnExit := OnEditControltExit;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyReferenceFlowValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMonthlyReferenceFlowValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption :=  'Monthly Reference Flows'; //FAppModules.Language.GetString('TabCaption.MonthlyDiversionFlow');
    Result := inherited LanguageHasChanged;

    MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[0, 0] := 'Date';
    MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[1, 0] := 'Obs. Flow (m3/s)';//'Flow (m3/s)';
    MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[2, 0] := 'Quality';
    MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[3, 0] := 'Ref. Flow (m3/s)';//'Factored Flow';

    MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[0, 0] := 'Year';
    MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[13, 0] := 'Total';

//    RepopulateDataViewer;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMonthlyReferenceFlowValidator.OnEditControltExit';
var
  LDailyDiversionGaugeData : TDiversionGauge;
begin
  inherited OnEditControltExit(Sender);
  try
    if (Sender = MonthlyReferenceFlowDialog.dtpDiversionDate) then
      UpdateDailyDataGrid;

    if (MonthlyReferenceFlowDialog.grdMonthlyThresholdValues = Sender) and (FThresholdValuesHasChanged) then
      UpdateDailyMonthlyThresholdValues;

    FThresholdValuesHasChanged := False;

    if Sender.ClassNameIs('TFieldEdit') then
    begin
      if TFieldEdit(Sender).HasValueChanged then
      begin
        LDailyDiversionGaugeData := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                                    DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
        if LDailyDiversionGaugeData <> nil then
        begin
          if (Sender = MonthlyReferenceFlowDialog.edtPlace) and
          (MonthlyReferenceFlowDialog.edtPlace.HasValueChanged) then
            UpdatePlace(LDailyDiversionGaugeData);
          if (Sender = MonthlyReferenceFlowDialog.edtLatidute) and
          (MonthlyReferenceFlowDialog.edtLatidute.HasValueChanged) then
            UpdateLatitude(LDailyDiversionGaugeData);
          if (Sender = MonthlyReferenceFlowDialog.edtLongitude) and
          (MonthlyReferenceFlowDialog.edtLongitude.HasValueChanged) then
            UpdateLongitude(LDailyDiversionGaugeData);
          if (Sender = MonthlyReferenceFlowDialog.edtCatchmentArea) and
          (MonthlyReferenceFlowDialog.edtCatchmentArea.HasValueChanged) then
            UpdateCatchmentArea(LDailyDiversionGaugeData);

          if (Sender = MonthlyReferenceFlowDialog.edtCatchmentFactor) and
          (MonthlyReferenceFlowDialog.edtCatchmentFactor.HasValueChanged) then
            UpdateCatchmentFactor(LDailyDiversionGaugeData);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.PopulateDataViewer;
const OPNAME = 'TMonthlyReferenceFlowValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyReferenceFlowValidator.RepopulateDataViewer;
const OPNAME = 'TMonthlyReferenceFlowValidator.RepopulateDataViewer';
var
  LDailyDiversionGaugeData : TDiversionGauge;
  LOldCursor : TCursor;
  LIndex : integer;
  LDataOption : string;
begin
  try
    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LockWindowUpdate(MonthlyReferenceFlowDialog.Handle);
    LDailyDiversionGaugeData := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                                DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if LDailyDiversionGaugeData <> nil then
    begin
      MonthlyReferenceFlowDialog.edtStationNo.SetFieldValue(LDailyDiversionGaugeData.StationNo);
      MonthlyReferenceFlowDialog.edtPlace.SetFieldValue(LDailyDiversionGaugeData.Place);
      MonthlyReferenceFlowDialog.edtLatidute.SetFieldValue(LDailyDiversionGaugeData.Latitude);
      MonthlyReferenceFlowDialog.edtLongitude.SetFieldValue(LDailyDiversionGaugeData.Longitude);
      MonthlyReferenceFlowDialog.edtCatchmentArea.SetFieldValue(LDailyDiversionGaugeData.CatchmentArea);
      MonthlyReferenceFlowDialog.edtCatchmentFactor.SetFieldValue(LDailyDiversionGaugeData.CatchmentScaleFactor);

      for LIndex := MinMonths to MaxMonths do
        MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.Cells[LIndex-1, 0] := CHydroMonthsDesc[LIndex];

      for LIndex := MinMonths to MaxMonths do
      begin
        MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.AddFieldProperty(
        FAppModules.FieldProperties.FieldProperty('ThresholdValue'));

        MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.Cells[LIndex-1,1] :=
        FloatToStr(LDailyDiversionGaugeData.ThresholdByMonth[CHydroMonths[LIndex]]);

        MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.Cells[LIndex-1,2] :=
        FormatFloat('##0',(LDailyDiversionGaugeData.ThresholdByMonth[CHydroMonths[LIndex]]/CDaysInMonth[CHydroMonths[LIndex]])*100);

        MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.IsRowEnabled[2] := False;
        MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.DisabledColor := clBtnFace;
      end;
      LDataOption := FAppModules.ViewIni.ReadString(ClassName,'DataOption','');
      if Trim(LDataOption) <> '' then
      begin
        MonthlyReferenceFlowDialog.rgbDailyDataQuality.ItemIndex := StrToInt(LDataOption);
        if StrToInt(LDataOption)= 2 then
          LDailyDiversionGaugeData.InfillGaps := True;
         if StrToInt(LDataOption)= 1 then
          LDailyDiversionGaugeData.ExcludeSuspectDailyData := True;
      end;

      case ActiveSubTab of
        actDailyGraph :   PopulateDailyGraph(LDailyDiversionGaugeData);
        actDailyGrid :  PopulateDailyGrid(LDailyDiversionGaugeData);
        actMonthlyGrid :  PopulateMonthlyGridData(LDailyDiversionGaugeData);
        actMonthlyGraph : PopulateMonthlyGraphData(LDailyDiversionGaugeData);
      end;
    end;
    LockWindowUpdate(0);
    Screen.Cursor := LOldCursor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.PopulateMonthlyGridData(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyReferenceFlowValidator.PopulateMonthlyGridData';
var
  LMonthlyFlowData : TMonthlyFlowData;
  LIndex : integer;
  LCount : integer;
  LYearTotal : double;
  LAvarages : array[1..12] of double;
  LGrandAvarage : double;
  LDailyDivesionProgressBar : TDailyDivesionProgressBar;  
begin
  try
    if (FMonthlyDataChanged) and (ADiversionGauge <> nil) and (ADiversionGauge.MonthlyDailyFlowCount > 0) then
    begin
      MonthlyReferenceFlowDialog.strgrdMonthlyFlow.RowCount := ADiversionGauge.MonthlyDailyFlowCount + 2;
      for LCount := MinMonths to MaxMonths do
        MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[LCount,0] := CHydroMonthsDesc[LCount];
      LDailyDivesionProgressBar := TDailyDivesionProgressBar.CreateWithoutDFM(nil, FAppModules);
      try
        LDailyDivesionProgressBar.ProgressPosition := 0;
        LDailyDivesionProgressBar.MaxProgress := ADiversionGauge.MonthlyDailyFlowCount;
        LDailyDivesionProgressBar.Initialise;
        LDailyDivesionProgressBar.ShowForm;

        for LIndex := 0 to ADiversionGauge.MonthlyDailyFlowCount -1 do
        begin
          LMonthlyFlowData := ADiversionGauge.MonthlyFlowDataByIndex[LIndex];
          LDailyDivesionProgressBar.ProgressPosition := LDailyDivesionProgressBar.ProgressPosition + 1;
          if LMonthlyFlowData <> nil then
          begin
            LYearTotal := 0;
            for LCount := MinMonths to MaxMonths do
            begin
              MonthlyReferenceFlowDialog.strgrdMonthlyFlow.ColWidths[0] := 85;
              MonthlyReferenceFlowDialog.strgrdMonthlyFlow.ColWidths[LCount] := 60;
              MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[0,LIndex+1] := IntToStr(LMonthlyFlowData.Year) +'/'+ IntToStr(LMonthlyFlowData.Year + 1);
              if LMonthlyFlowData.AvgFlowByIndex[LCount] = NullFloat then
                MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[LCount,LIndex+1] := ''
              else
              begin
                MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[LCount,LIndex+1] :=
                FormatFloat('0.000',LMonthlyFlowData.AvgFlowByIndex[LCount]);

                LYearTotal := LYearTotal + LMonthlyFlowData.AvgFlowByIndex[LCount];
                LAvarages[LCount] := LAvarages[LCount] + LMonthlyFlowData.AvgFlowByIndex[LCount];
              end;
            end;

            MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[13,LIndex+1] := FormatFloat('0.000',LYearTotal);
          end;
        end;
        LGrandAvarage := 0;
        for LCount := Low(LAvarages) to High(LAvarages) do
        begin
          LAvarages[LCount] := LAvarages[LCount] / ADiversionGauge.MonthlyDailyFlowCount;
          MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[LCount,MonthlyReferenceFlowDialog.strgrdMonthlyFlow.RowCount-1] := FormatFloat('####0.000',LAvarages[LCount]);
          MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[0,MonthlyReferenceFlowDialog.strgrdMonthlyFlow.RowCount-1] := 'Average';
          LGrandAvarage := LGrandAvarage + LAvarages[LCount];
        end;
        LGrandAvarage := LGrandAvarage;
        MonthlyReferenceFlowDialog.strgrdMonthlyFlow.Cells[13,MonthlyReferenceFlowDialog.strgrdMonthlyFlow.RowCount-1] := FormatFloat('####0.000',LGrandAvarage);
        FMonthlyDataChanged := False;
      finally
        FreeAndNil(LDailyDivesionProgressBar)
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.PopulateMonthlyGraphData(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyReferenceFlowValidator.PopulateMonthlyGraphData';
var
  LMonthlyFlowData : TMonthlyFlowData;
  LIndex           : integer;
  LCount           : integer;
  LDayLabel        : string;
  LDailyDivesionProgressBar : TDailyDivesionProgressBar;
begin
  try
    if (FMonthlyGraphDataChanged) and (ADiversionGauge <> nil) then
    begin
      LDailyDivesionProgressBar := TDailyDivesionProgressBar.CreateWithoutDFM(nil, FAppModules);
      try
        LDailyDivesionProgressBar.ProgressPosition := 0;
        LDailyDivesionProgressBar.MaxProgress := ADiversionGauge.MonthlyDailyFlowCount;
        LDailyDivesionProgressBar.Initialise;
        LDailyDivesionProgressBar.ShowForm;

        MonthlyReferenceFlowDialog.MonthlyLineSeries.Clear;
        for LIndex := 0 to ADiversionGauge.MonthlyDailyFlowCount -1 do
        begin
          LMonthlyFlowData := ADiversionGauge.MonthlyFlowDataByIndex[LIndex];
          LDailyDivesionProgressBar.ProgressPosition := LDailyDivesionProgressBar.ProgressPosition + 1;
          if LMonthlyFlowData <> nil then
          begin
            for LCount := MinMonths to MaxMonths do
            begin
              LDayLabel := FormatDateTime('yyyy/mm',(EncodeDate(LMonthlyFlowData.Year,LCount,01)));
              if LMonthlyFlowData.AvgFlowByIndex[LCount] <> NullFloat then
                MonthlyReferenceFlowDialog.MonthlyLineSeries.AddY(LMonthlyFlowData.AvgFlowByIndex[LCount],
                                                              LDayLabel, clTeeColor);
            end;
          end;
        end;
        FMonthlyGraphDataChanged := False;
      finally
        FreeAndNil(LDailyDivesionProgressBar)
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.PopulateDailygraph(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyReferenceFlowValidator.PopulateDailygraph';
var
  LDayLabel     : string;
  LInfilledGap,
  LDailyData    : TDailyFlowData;
  LIndex        : integer;
  LAvgFlow      : double;
begin
  try
    if (FDailyGraphDataChanged) and (ADiversionGauge <> nil) then
    begin
      if ADiversionGauge.DailyDataCount > 0 then
      begin
        MonthlyReferenceFlowDialog.DailyLineSeries.Clear;
        for LIndex := 0 to ADiversionGauge.DailyDataCount -1 do
        begin
          LDailyData := ADiversionGauge.DailyFlowDataByIndex[LIndex];
          if LDailyData <> nil then
          begin
            if (LDailyData.AvgFlow = NullFloat) then
            begin
              LAvgFlow := 0.0;
              case MonthlyReferenceFlowDialog.rgbDailyDataQuality.ItemIndex of
              2 :
                begin
                  LInfilledGap := ADiversionGauge.GetInfilledDailyDataByDate(LDailyData.DiversionDate);
                  MonthlyReferenceFlowDialog.DailyLineSeries.AddY(LInfilledGap.AvgFlow, DateToStr(LInfilledGap.DiversionDate), clTeeColor);
                  Continue;
                end;
              end;
            end
            else
              LAvgFlow := LDailyData.AvgFlow;
            LDayLabel := DateToStr(LDailyData.DiversionDate);
            MonthlyReferenceFlowDialog.DailyLineSeries.AddY(LAvgFlow, LDayLabel, clTeeColor);
          end;
        end;
        FDailyGraphDataChanged := False;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.PopulateDailyGrid(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyReferenceFlowValidator.PopulateDailyGrid';
var
  LDiversionDate,
  LMessage                : string;
  LInfilledGap,
  LDailyData              : TDailyFlowData;
  LIndex                  : integer;
  LFieldProperty          : TAbstractFieldProperty;
  LHydroYear,
  LYear,LMonth,LDay       : word;
  LDataWithoutCodeFound,
  LReplaceDataWithoutCode : boolean;
begin
  try
    if (FDailyDataChanged) and (ADiversionGauge <> nil) then
    begin
      if ADiversionGauge.DailyDataCount > 0 then
      begin
        MonthlyReferenceFlowDialog.strgrdDailyFlow.RowCount := ADiversionGauge.DailyDataCount + 1;
        LDataWithoutCodeFound   := False;
        LReplaceDataWithoutCode := False;
        for LIndex := 0 to ADiversionGauge.DailyDataCount -1 do
        begin
          LDailyData := ADiversionGauge.DailyFlowDataByIndex[LIndex];
          if LDailyData <> nil then
          begin
            LDiversionDate := DateToStr(LDailyData.DiversionDate);

            LFieldProperty := FAppModules.FieldProperties.FieldProperty('DiversionDate');
            MonthlyReferenceFlowDialog.strgrdDailyFlow.AddFieldProperty(LFieldProperty);
            MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[0,LIndex + 1] := LDiversionDate;

            if (LDailyData.AvgFlow = NullFloat) then
            begin
              case MonthlyReferenceFlowDialog.rgbDailyDataQuality.ItemIndex of
                2 :
                begin
                  LInfilledGap := ADiversionGauge.GetInfilledDailyDataByDate(LDailyData.DiversionDate);
                  if (LInfilledGap <> nil)  then
                  begin
                    Decodedate(LInfilledGap.DiversionDate,LYear,LMonth,LDay);
                    if LMonth < 10 then
                      LHydroYear := LYear -1
                    else
                      LHydroYear := LYear;
                    if (ADiversionGauge.NumberOfGabsByMonth[LHydroYear,LMonth] <= ADiversionGauge.ThresholdByMonth[LMonth]) then
                    begin
                      LFieldProperty := FAppModules.FieldProperties.FieldProperty('StationNo');
                      MonthlyReferenceFlowDialog.strgrdDailyFlow.AddFieldProperty(LFieldProperty);
                      MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[1,LIndex + 1] :=
                      FormatFloat('0.000',LInfilledGap.AvgFlow);

                      LFieldProperty := FAppModules.FieldProperties.FieldProperty('QualityCode');
                      MonthlyReferenceFlowDialog.strgrdDailyFlow.AddFieldProperty(LFieldProperty);
                      MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[2,LIndex + 1] := IntToStr(LInfilledGap.QualityCode);
                    end;
                  end;
                end
                else
                begin
                  LFieldProperty := FAppModules.FieldProperties.FieldProperty('AvgFlow');
                  MonthlyReferenceFlowDialog.strgrdDailyFlow.AddFieldProperty(LFieldProperty);
                  MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[1,LIndex + 1] := '';

                  LFieldProperty := FAppModules.FieldProperties.FieldProperty('QualityCode');
                  MonthlyReferenceFlowDialog.strgrdDailyFlow.AddFieldProperty(LFieldProperty);
                  if(LDailyData.QualityCode = 0) then
                  begin
                    if not LDataWithoutCodeFound then
                    begin
                      LMessage := FAppModules.Language.GetString('TMonthlyReferenceFlowValidator.ReplaceDataWithoutCode');;
                      if MessageDlg(LMessage,mtWarning,mbOKCancel,0) = mrOk then
                        LReplaceDataWithoutCode := True;
                      LDataWithoutCodeFound := True;
                    end;

                    if LReplaceDataWithoutCode then
                      LDailyData.QualityCode := 170;
                  end;
                  MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[2,LIndex + 1] := IntToStr(LDailyData.QualityCode);
                end;
              end;
            end
            else
            begin
              LFieldProperty := FAppModules.FieldProperties.FieldProperty('AvgFlow');
              MonthlyReferenceFlowDialog.strgrdDailyFlow.AddFieldProperty(LFieldProperty);
              MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[1,LIndex + 1] :=
              FormatFloat('0.000',LDailyData.AvgFlow);

              LFieldProperty := FAppModules.FieldProperties.FieldProperty('QualityCode');
              MonthlyReferenceFlowDialog.strgrdDailyFlow.AddFieldProperty(LFieldProperty);
              MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[2,LIndex + 1] := IntToStr(LDailyData.QualityCode);
            end;

            LFieldProperty := FAppModules.FieldProperties.FieldProperty('StationNo');
            MonthlyReferenceFlowDialog.strgrdDailyFlow.AddFieldProperty(LFieldProperty);
            MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[3,LIndex + 1] :=
            FormatFloat('0.000',LDailyData.FactoredFlow);
          end;
        end;
        FDailyDataChanged := False;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.UpdatePlace(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyReferenceFlowValidator.UpdatePlace';
var
  LMessage    : string;
  LFieldValue : string;
begin
  try
    if (ADiversionGauge <> nil) then
    begin
      MonthlyReferenceFlowDialog.edtPlace.FieldValidationError :='';
      LFieldValue := MonthlyReferenceFlowDialog.edtPlace.Text;
      if (FAppModules.FieldProperties.ValidateFieldProperty('Place',
          LFieldValue, LMessage)) then
      begin
        ADiversionGauge.Place := LFieldValue;
        PopulateDataViewer;
        DoContextValidation(dvtPlace);
      end
      else
        MonthlyReferenceFlowDialog.edtPlace.FieldValidationError := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.UpdateLatitude(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyReferenceFlowValidator.UpdateLatitude';
var
  LMessage    : string;
  LFieldValue : string;
begin
  try
    if (ADiversionGauge <> nil) then
    begin
      MonthlyReferenceFlowDialog.edtLatidute.FieldValidationError :='';
      LFieldValue := MonthlyReferenceFlowDialog.edtLatidute.Text;
      if (FAppModules.FieldProperties.ValidateFieldProperty('Latitude',
          LFieldValue, LMessage)) then
      begin
        ADiversionGauge.Latitude := LFieldValue;
        PopulateDataViewer;
        DoContextValidation(dvtLatitude);
      end
      else
        MonthlyReferenceFlowDialog.edtLatidute.FieldValidationError := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.UpdateLongitude(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyReferenceFlowValidator.UpdateLongitude';
var
  LMessage    : string;
  LFieldValue : string;
begin
  try
    if (ADiversionGauge <> nil) then
    begin
      MonthlyReferenceFlowDialog.edtLongitude.FieldValidationError :='';
      LFieldValue := MonthlyReferenceFlowDialog.edtLongitude.Text;
      if (FAppModules.FieldProperties.ValidateFieldProperty('Longitude',
          LFieldValue, LMessage)) then
      begin
        ADiversionGauge.Longitude := LFieldValue;
        PopulateDataViewer;
        DoContextValidation(dvtLongitude);
      end
      else
        MonthlyReferenceFlowDialog.edtLongitude.FieldValidationError := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.UpdateCatchmentArea(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyReferenceFlowValidator.UpdateCatchmentArea';
var
  LMessage    : string;
  LFieldValue : string;
begin
  try
    if (ADiversionGauge <> nil) then
    begin
      MonthlyReferenceFlowDialog.edtCatchmentArea.FieldValidationError :='';
      LFieldValue := MonthlyReferenceFlowDialog.edtCatchmentArea.Text;
      if (FAppModules.FieldProperties.ValidateFieldProperty('CatchmentArea',
          LFieldValue, LMessage)) then
      begin
        ADiversionGauge.CatchmentArea := StrToFloat(LFieldValue);
        PopulateDataViewer;
        DoContextValidation(dvtCatchmentArea);
      end
      else
        MonthlyReferenceFlowDialog.edtCatchmentArea.FieldValidationError := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.UpdateCatchmentFactor(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyReferenceFlowValidator.UpdateCatchmentFactor';
var
  LMessage    : string;
  LFieldValue : string;
begin
  try
    if (ADiversionGauge <> nil) then
    begin
      MonthlyReferenceFlowDialog.edtCatchmentFactor.FieldValidationError :='';
      LFieldValue := MonthlyReferenceFlowDialog.edtCatchmentFactor.Text;
      if (FAppModules.FieldProperties.ValidateFieldProperty('CatchmentScaleFactor',
          LFieldValue, LMessage)) then
      begin
        ADiversionGauge.CatchmentScaleFactor := StrToFloat(LFieldValue);
        PopulateDataViewer;
        DoContextValidation(dvtCatchmentScaleFactor);
      end
      else
        MonthlyReferenceFlowDialog.edtCatchmentFactor.FieldValidationError := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.UpdateDailyMonthlyThresholdValues;
const OPNAME = 'TMonthlyReferenceFlowValidator.UpdateDailyMonthlyThresholdValues';
var
  LMessage,
  LValue : string;
  LDiversionGauge: TDiversionGauge;
  LCol : integer;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if LDiversionGauge <> nil then
    begin
      for LCol := MinMonths to MaxMonths do
      begin
        LValue := MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.Cells[LCol-1,1];
        MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.ValidationError[LCol,1, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty('ThresholdValue', LValue,LMessage,LCol)) then
        begin
          LDiversionGauge.ThresholdByMonth[LCol] := StrToFloat(LValue);
        end
        else
          MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.ValidationError[LCol,1, gveCellContext] := LMessage;
      end;
      PopulateDataViewer;
      DoContextValidation(dvtCompensationValue);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyReferenceFlowValidator.GetMonthlyReferenceFlowDialog : TMonthlyReferenceFlowDialog;
const OPNAME = 'TMonthlyReferenceFlowValidator.GetMonthlyReferenceFlowDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TMonthlyReferenceFlowDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMonthlyReferenceFlowValidator.SaveState: boolean;
const OPNAME = 'TMonthlyReferenceFlowValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyReferenceFlowValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                      ANewValue: string): boolean;
const OPNAME = 'TMonthlyReferenceFlowValidator.StudyDataHasChanged';
var
  LDiversionGauge : TDiversionGauge;
  LMonthlyFlowData : TMonthlyFlowData;
  LYear,LMonth,LDay : word;
  LDiversionDate : TDateTime;
  LHydroYear : integer;
//  LNonSuspectDailyFlowData,
  LDailyFlowData : TDailyFlowData;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                         DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];

    if (AFieldName = 'DiversionDate') or (AFieldName = 'AvgFlow') or (AFieldName =  'StationNo')
       or (AFieldName = 'QualityCode') or (AFieldName = 'CatchmentScaleFactor')
       or (AFieldName = 'ThresholdValue') then
    begin
      FDailyDataChanged := True;
      FDailyGraphDataChanged := True;
      FMonthlyGraphDataChanged := True;
      FMonthlyDataChanged := True;
    end;

    if (AContext = sdccEdit) and (AFieldName = 'AvgFlow') then
    begin
      LDiversionDate := StrToDate(MonthlyReferenceFlowDialog.strgrdDailyFlow.cells[0,
                                  MonthlyReferenceFlowDialog.strgrdDailyFlow.Row]);
      if LDiversionGauge <> nil then
      begin
        DecodeDate(LDiversionDate,LYear,LMonth,LDay);
        if LMonth < 10 then
          LHydroYear := LYear -1
        else
          LHydroYear := LYear;
        LMonthlyFlowData := LDiversionGauge.MonthlyFlowDataByYear[LHydroYear];
        if LMonthlyFlowData <> nil then
        begin
          LMonthlyFlowData.IncludeAllMonthTotalByMonth[LMonth] := LMonthlyFlowData.IncludeAllMonthTotalByMonth[LMonth]-StrToFloat(AOldValue);
          LMonthlyFlowData.IncludeAllMonthTotalByMonth[LMonth] := LMonthlyFlowData.IncludeAllMonthTotalByMonth[LMonth]+StrToFloat(ANewValue);

          LDailyFlowData := LDiversionGauge.GetDailyFlowDataByDate(LDiversionDate);
          if LDailyFlowData <> nil then
          begin
            if not (LDailyFlowData.QualityCode in [1,2,3,4,5,6,7,50,60,65,66,91,150]) then
            begin
              LMonthlyFlowData.SuspectMonthTotalByMonth[LMonth] := LMonthlyFlowData.SuspectMonthTotalByMonth[LMonth] + StrToFloat(ANewValue);
            end;
          end;

        end;
      end;
    end;

    if (AContext = sdccDelete) and (AFieldName = 'AvgFlow') then
    begin
      LDiversionDate := StrToDate(MonthlyReferenceFlowDialog.strgrdDailyFlow.cells[0,
                                  MonthlyReferenceFlowDialog.strgrdDailyFlow.Row]);
      if LDiversionGauge <> nil then
      begin
        DecodeDate(LDiversionDate,LYear,LMonth,LDay);
        if LMonth < 10 then
          LHydroYear := LYear -1
        else
          LHydroYear := LYear;
        LMonthlyFlowData := LDiversionGauge.MonthlyFlowDataByYear[LHydroYear];
        if LMonthlyFlowData <> nil then
          LMonthlyFlowData.IncludeAllMonthTotalByMonth[LMonth] := LMonthlyFlowData.IncludeAllMonthTotalByMonth[LMonth] - StrToFloat(AOldValue);
      end;
    end;

    if ((AContext = sdccEdit) and (AFieldName = 'QualityCode')) then
    begin
      LDiversionDate := StrToDate(MonthlyReferenceFlowDialog.strgrdDailyFlow.cells[0,
                                  MonthlyReferenceFlowDialog.strgrdDailyFlow.Row]);
      if LDiversionGauge <> nil then
      begin
        DecodeDate(LDiversionDate,LYear,LMonth,LDay);
        if LMonth < 10 then
          LHydroYear := LYear -1
        else
          LHydroYear := LYear;
        LMonthlyFlowData := LDiversionGauge.MonthlyFlowDataByYear[LHydroYear];
        if LMonthlyFlowData <> nil then
        begin
          LDailyFlowData := LDiversionGauge.GetDailyFlowDataByDate(LDiversionDate);
          if LDailyFlowData <> nil then
          begin

            {if (StrToFloat(ANewValue) <= 7) and (StrToFloat(AOldValue) > 7) and
              (LDailyFlowData.AvgFlow <> NullFloat) then
            begin
              LNonSuspectDailyFlowData := LDiversionGauge.AddNonSuspectDailyFlowData;
              LNonSuspectDailyFlowData.Initialise;
              LNonSuspectDailyFlowData.Poulate(LDailyFlowData.StationID,LDailyFlowData.Identifier,
                                               LDailyFlowData.DiversionDate,LDailyFlowData.AvgFlow,
                                               LDiversionGauge.CatchmentScaleFactor,StrToInt(ANewValue));
            end
            else
            if (StrToFloat(ANewValue) > 7) and (StrToFloat(AOldValue) <= 7) and
               (LDailyFlowData.AvgFlow <> NullFloat) then
              LDiversionGauge.DeleteDailyFlowDataById(LDailyFlowData.Identifier);
            }
            TDailyDiversionDataObject(FAppModules.Model.ModelData).RefreshCalculatedData;
          end
        end;

      end;
    end;

    if (AFieldName = 'OldStationNo') then
      MonthlyReferenceFlowDialog.edtStationNo.Text := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                                                      DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier].StationNo;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyReferenceFlowValidator.StudyHasChanged: boolean;
const OPNAME = 'TMonthlyReferenceFlowValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyReferenceFlowValidator.DoAddAddDailyDataClick(Sender: TObject);
const OPNAME = 'TMonthlyReferenceFlowValidator.DoAddAddDailyDataClick';
var
  LDiversionGauge : TDiversionGauge;
  LDailyFlowData : TDailyFlowData;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if LDiversionGauge <> nil then
    begin
      LDailyFlowData := LDiversionGauge.NewDailyFlowData;
      if LDailyFlowData <> nil then
      begin
        PopulateDataViewer;
        DoContextValidation(dvtMonthlyReference);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.DoDeleteDailyDataClick(Sender: TObject);
const OPNAME = 'TMonthlyReferenceFlowValidator.DoDeleteDailyDataClick';
var
  LDiversionGauge : TDiversionGauge;
  LDailyFlowData : TDailyFlowData;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if (LDiversionGauge <> nil) and (LDiversionGauge.DailyDataCount > 0) then
    begin
      LDailyFlowData := LDiversionGauge.DailyFlowDataByIndex[MonthlyReferenceFlowDialog.strgrdDailyFlow.Row-1];
      if LDailyFlowData <> nil then
      begin
        if LDiversionGauge.RemoveDailyData(LDailyFlowData.Identifier) then
        begin
          PopulateDataViewer;
          DoContextValidation(dvtMonthlyReference);
        end;
      end;  
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.UpdateDailyDataGrid;
const OPNAME = 'TMonthlyReferenceFlowValidator.UpdateDailyDataGrid';
var
  LMessage : string;
  LDiversionGauge : TDiversionGauge;
  LDailyFlowData : TDailyFlowData;
  LValue : string;
  LAvgValue : double;
  LRow : integer;
  LDivesionDate : TDateTime;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    LValue := MonthlyReferenceFlowDialog.strgrdDailyFlow.Cells[MonthlyReferenceFlowDialog.strgrdDailyFlow.Col,
                                                               MonthlyReferenceFlowDialog.strgrdDailyFlow.Row];
    LRow := MonthlyReferenceFlowDialog.strgrdDailyFlow.Row;

    LDailyFlowData := LDiversionGauge.DailyFlowDataByIndex[LRow-1];
    if (LDiversionGauge <> nil) and (LDailyFlowData <> nil) then
    begin
      if (MonthlyReferenceFlowDialog.strgrdDailyFlow.Col = 0 ) and
       (MonthlyReferenceFlowDialog.strgrdDailyFlow.Row > 0) then
      begin
        LDivesionDate := 0;
        if (MonthlyReferenceFlowDialog.dtpDiversionDate.DateTime > 0) then
          LDivesionDate := MonthlyReferenceFlowDialog.dtpDiversionDate.DateTime;
        if LDivesionDate <= 0 then
          Exit;

        MonthlyReferenceFlowDialog.strgrdDailyFlow.ValidationError[0, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'DiversionDate', DateToStr(LDivesionDate),LMessage)) then
        begin
          LDailyFlowData.DiversionDate := LDivesionDate;
          PopulateDataViewer;
          DoContextValidation(dvtDiversionDate);
        end
        else
          MonthlyReferenceFlowDialog.strgrdDailyFlow.ValidationError[0, LRow, gveCellContext] := LMessage;
      end;

      if (MonthlyReferenceFlowDialog.strgrdDailyFlow.Col = 1 ) and
       (MonthlyReferenceFlowDialog.strgrdDailyFlow.Row > 0) then
      begin
        if Trim(LValue) = '' then
          LAvgValue := NullFloat
        else
          LAvgValue := StrToFloat(LValue);
        MonthlyReferenceFlowDialog.strgrdDailyFlow.ValidationError[1, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'AvgFlow', FloatToStr(LAvgValue),LMessage)) then
        begin
          LDailyFlowData.AvgFlow := LAvgValue;
          PopulateDataViewer;
          DoContextValidation(dvtAvgFlow);
        end
        else
          MonthlyReferenceFlowDialog.strgrdDailyFlow.ValidationError[1, LRow, gveCellContext] := LMessage;
      end;

      if (MonthlyReferenceFlowDialog.strgrdDailyFlow.Col = 2 ) and
       (MonthlyReferenceFlowDialog.strgrdDailyFlow.Row > 0) then
      begin
        MonthlyReferenceFlowDialog.strgrdDailyFlow.ValidationError[1, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'QualityCode', LValue,LMessage)) then
        begin
          LDailyFlowData.QualityCode := StrToInt(LValue);
          PopulateDataViewer;
          DoContextValidation(dvtQualityCode);
        end
        else
          MonthlyReferenceFlowDialog.strgrdDailyFlow.ValidationError[1, LRow, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMonthlyReferenceFlowValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    if (MonthlyReferenceFlowDialog.strgrdDailyFlow = ASender) then
      UpdateDailyDataGrid;
    if (MonthlyReferenceFlowDialog.grdMonthlyThresholdValues = ASender) then
      FThresholdValuesHasChanged := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyReferenceFlowValidator.OnstrgrdDailyFlowSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TMonthlyReferenceFlowValidator.OnstrgrdDailyFlowSelectCell';
begin
  try
    if ACol = 0 then
    begin
      MonthlyReferenceFlowDialog.dtpDiversionDate.Top  := 2 +MonthlyReferenceFlowDialog.strgrdDailyFlow.Top +
                                                          ((1 + MonthlyReferenceFlowDialog.strgrdDailyFlow.DefaultRowHeight) *
                                                          (ARow - MonthlyReferenceFlowDialog.strgrdDailyFlow.TopRow + 1));
      MonthlyReferenceFlowDialog.dtpDiversionDate.Visible := True;
      MonthlyReferenceFlowDialog.dtpDiversionDate.IsEnabled := True;
      MonthlyReferenceFlowDialog.dtpDiversionDate.DateTime := StrtoDate(MonthlyReferenceFlowDialog.strgrdDailyFlow.cells[ACol, ARow]);
      if (MonthlyReferenceFlowDialog.strgrdDailyFlow.ValidationError[ACol, ARow, gveCellContext] <> '') then
      begin
        MonthlyReferenceFlowDialog.dtpDiversionDate.ValidationError   :=
        MonthlyReferenceFlowDialog.strgrdDailyFlow.ValidationError[ACol, ARow, gveCellContext];
        MonthlyReferenceFlowDialog.dtpDiversionDate.InValidationError := True;
        MonthlyReferenceFlowDialog.dtpDiversionDate.ShowErrorState(True);
      end
      else
      begin
        MonthlyReferenceFlowDialog.dtpDiversionDate.ValidationError   := '';
        MonthlyReferenceFlowDialog.dtpDiversionDate.InValidationError := False;
        MonthlyReferenceFlowDialog.dtpDiversionDate.ShowErrorState(False);
      end;
    end
    else
      MonthlyReferenceFlowDialog.dtpDiversionDate.Visible := False;

      
except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyReferenceFlowValidator.OnstrgrdDailyFlowTopLeftChanged(Sender: TObject);
const OPNAME = 'TMonthlyReferenceFlowValidator.OnstrgrdDailyFlowTopLeftChanged';
begin
  try
    MonthlyReferenceFlowDialog.dtpDiversionDate.Visible := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{procedure TMonthlyReferenceFlowValidator.DorgbThresholdClick(Sender: TObject);
const OPNAME = 'TMonthlyReferenceFlowValidator.DorgbThresholdClick';
begin
  try
    if MonthlyReferenceFlowDialog.rgbThreshold.Visible then
    begin
      case MonthlyReferenceFlowDialog.rgbThreshold.ItemIndex of
        0 :
        begin
          MonthlyReferenceFlowDialog.lblThreshold.Caption := 'Day(s)';
          if Trim(MonthlyReferenceFlowDialog.edtThreshold.Text) <> '' then
  //          MonthlyReferenceFlowDialog.edtThreshold.Text := Trunc((StrToFloat(MonthlyReferenceFlowDialog.edtThreshold.Text)/100)*30)/
        end;
        1 :
        begin
          MonthlyReferenceFlowDialog.lblThreshold.Caption := '%';
          if Trim(MonthlyReferenceFlowDialog.edtThreshold.Text) <> '' then
//            MonthlyReferenceFlowDialog.edtThreshold.Text := Trunc((/100)*StrToFloat(MonthlyReferenceFlowDialog.edtThreshold.Text))
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}
procedure TMonthlyReferenceFlowValidator.DorgbDailyDataQualityClick(Sender: TObject);
const OPNAME = 'TMonthlyReferenceFlowValidator.DorgbDailyDataQualityClick';
var
  LDataOption : string;
  LDiversionGauge : TDiversionGauge;
  LOldCursor             : TCursor;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if LDiversionGauge <> nil then
    begin
      case MonthlyReferenceFlowDialog.rgbDailyDataQuality.ItemIndex of
        0 :
        begin
          LDiversionGauge.IncludeAll := True;
          LDiversionGauge.ExcludeSuspectDailyData := False;
          LDiversionGauge.InfillGaps := False;
          MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.Visible := False;
          MonthlyReferenceFlowDialog.lblThreshold.Visible := False;
          MonthlyReferenceFlowDialog.lblThresholdDays.Visible := False;
          MonthlyReferenceFlowDialog.lblThresholdPercentage.Visible := False;
        end;
        1 :
        begin
          LDiversionGauge.IncludeAll := False;
          LDiversionGauge.ExcludeSuspectDailyData := True;
          LDiversionGauge.InfillGaps := False;
          MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.Visible := False;
          MonthlyReferenceFlowDialog.lblThreshold.Visible := False;
          MonthlyReferenceFlowDialog.lblThresholdDays.Visible := False;
          MonthlyReferenceFlowDialog.lblThresholdPercentage.Visible := False;
        end;
        2 :
        begin
          LDiversionGauge.IncludeAll := False;
          LDiversionGauge.ExcludeSuspectDailyData := False;
          LDiversionGauge.InfillGaps := True;
          MonthlyReferenceFlowDialog.grdMonthlyThresholdValues.Visible := True;
          MonthlyReferenceFlowDialog.lblThreshold.Visible := True;;
          MonthlyReferenceFlowDialog.lblThresholdDays.Visible := True;
          MonthlyReferenceFlowDialog.lblThresholdPercentage.Visible := True;
        end;
      end;
    end;
    LDataOption := IntToStr(MonthlyReferenceFlowDialog.rgbDailyDataQuality.ItemIndex);
    FAppModules.ViewIni.WriteString(ClassName,'DataOption',LDataOption);
    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LockWindowUpdate(MonthlyReferenceFlowDialog.Handle);
    FAppModules.Model.StudyDataHasChanged(sdccEdit,'StationNo','','');
    PopulateDataViewer;
    LockWindowUpdate(0);
    Screen.Cursor := LOldCursor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyReferenceFlowValidator.CanExport: boolean;
const OPNAME = 'TMonthlyReferenceFlowValidator.CanExport';
begin
  Result := False;
  try
    if(MonthlyReferenceFlowDialog <> nil) then
      Result := MonthlyReferenceFlowDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyReferenceFlowValidator.CanPrint: boolean;
const OPNAME = 'TMonthlyReferenceFlowValidator.CanPrint';
begin
  Result := False;
  try
    if(MonthlyReferenceFlowDialog <> nil) then
      Result := MonthlyReferenceFlowDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyReferenceFlowValidator.DoExport(AFileName: string);
const OPNAME = 'TMonthlyReferenceFlowValidator.DoExport';
begin
  try
    if(MonthlyReferenceFlowDialog <> nil) then
      MonthlyReferenceFlowDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyReferenceFlowValidator.DoPrint;
const OPNAME = 'TMonthlyReferenceFlowValidator.DoPrint';
begin
  try
    if(MonthlyReferenceFlowDialog <> nil) then
      MonthlyReferenceFlowDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
