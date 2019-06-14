//
//
//  UNIT      : Contains TMonthlyDiversionFlowValidator Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 20/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//


unit UMonthlyDiversionFlowValidator;

interface
uses
  Classes,
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,

  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  VCL.Graphics,
  VCL.Dialogs,
  Math,
  VCL.Forms,
  Windows,
  Contnrs,
  VCL.Grids,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UDataComponent,
  UDailyDiversionDataObject,
  UDailyDiversionGaugeData,
  UMonthlyDiversionFlowDialog,
  UDailyDiversionContextValidationType,
  UGenericModelLinkClasses;
type
  TIFRActiveSubTab = (actIFRDailyGrid,actIFRDailyGraph,actIFRMonthlyGrid,actIFRMonthlyGraph);
  TMonthlyDiversionFlowValidator = class (TAbstractDataDialogValidator)
  protected
    FIFRActiveSubTab              : TIFRActiveSubTab;
    FCompensationValuesHasChanged,
    FDailyGraphDataChanged,
    FMonthlyDataChanged,
    FMonthlyGraphDataChanged,
    FDailyDataChanged             : boolean;
    procedure CreateMemberObjects; override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged(ASender: TObject;ACol: Integer; ARow: Integer); override;
    procedure OnstrgrdDailyInstreamFlowSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OnstrgrdDailyInstreamFlowTopLeftChanged(Sender: TObject);

    procedure UpdateDailyMonthlyCompensationValues;
    procedure UpdateCapacityOfDiversion(ADiversionGauge : TDiversionGauge);
    procedure UpdateScaleFactor(ADiversionGauge : TDiversionGauge);
    procedure UpdateDailyInstreamDataGrid;
    procedure UpdatergrpImportIFR(ADiversionGauge : TDiversionGauge);

    procedure DoAddDailyInstreamData(Sender: TObject);
    procedure DoDeleteDailyInstreamData(Sender: TObject);
    procedure OnTabChanged(Sender: Tobject);
    procedure RepopulateDataViewer;
    function GetMonthlyDiversionFlowDialog : TMonthlyDiversionFlowDialog;
    procedure PopulateDailyInstreamGraph(ADiversionGauge : TDiversionGauge);
    procedure PopulateDailyInstreamGrid(ADiversionGauge : TDiversionGauge);
    procedure PopulateMonthlyGridData(ADiversionGauge : TDiversionGauge);
    procedure PopulateMonthlyGraphData(ADiversionGauge : TDiversionGauge);

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

    property IFRActiveSubTab : TIFRActiveSubTab read FIFRActiveSubTab write FIFRActiveSubTab;
    property MonthlyDiversionFlowDialog : TMonthlyDiversionFlowDialog read GetMonthlyDiversionFlowDialog;
end;
implementation
uses
  SysUtils,
  UConstants,
  UDataEditComponent,
  UDailyDivesionProgressBar,
  UDailyDiversionGUIManager,
  UDailyIFRDataValidator,
  UErrorHandlingOperations;
{ DailyDiversionValidator }


procedure TMonthlyDiversionFlowValidator.OnTabChanged(Sender: Tobject);
const OPNAME = 'TMonthlyDiversionFlowValidator.OnTabChanged';
begin
  try
    if MonthlyDiversionFlowDialog.pgcInstreamFlow.ActivePage =
      MonthlyDiversionFlowDialog.tbsDailyInstreamFlowGraph  then
      IFRActiveSubTab := actIFRDailyGraph;

    if MonthlyDiversionFlowDialog.pgcInstreamFlow.ActivePage =
      MonthlyDiversionFlowDialog.tbsDailyInstreamFlowGrid  then
      IFRActiveSubTab := actIFRDailyGrid;
    if MonthlyDiversionFlowDialog.pgcInstreamFlow.ActivePage =
      MonthlyDiversionFlowDialog.tbsMonthlyInstreamFlowGrid  then
      IFRActiveSubTab := actIFRMonthlyGrid;
    if MonthlyDiversionFlowDialog.pgcInstreamFlow.ActivePage =
      MonthlyDiversionFlowDialog.tbsMonthlyInstreamFlowGraph  then
      IFRActiveSubTab := actIFRMonthlyGraph;

    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMonthlyDiversionFlowValidator.ClearDataViewer;
const OPNAME = 'TMonthlyDiversionFlowValidator.ClearDataViewer';
var
  LRow    : integer;
  LCol    : integer;
begin
  inherited ClearDataViewer;
  try
    if FDailyDataChanged then
    begin
      for LCol := 0 to MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ColCount -1 do
      begin
        for LRow := 1 to MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.RowCount -1 do
          MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[LCol,LRow] := '';
      end;
      MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ColCount         := 6;
      MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.RowCount         := 2;
    end;

    if FMonthlyDataChanged then
    begin
      for LCol := 0 to MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.ColCount -1 do
      begin
        for LRow := 1 to MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.RowCount -1 do
          MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[LCol,LRow] := '';
      end;
      MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.ColCount         := 14;
      MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.RowCount         := 2;
    end;
    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDiversionFlowValidator.CreateMemberObjects;
const OPNAME = 'TMonthlyDiversionFlowValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TMonthlyDiversionFlowDialog.Create(nil, FAppModules);

    MonthlyDiversionFlowDialog.edtCapacityOfDiversion.FieldProperty := FAppModules.FieldProperties.FieldProperty('CapacityOfDiversion');
    MonthlyDiversionFlowDialog.edtCapacityOfDiversion.OnEnter       := OnEditControlEnter;
    MonthlyDiversionFlowDialog.edtCapacityOfDiversion.OnExit        := OnEditControltExit;
    MonthlyDiversionFlowDialog.edtCapacityOfDiversion.IsEnabled     := True;

    MonthlyDiversionFlowDialog.edtScaleFactor.FieldProperty := FAppModules.FieldProperties.FieldProperty('InstreamScaleFactor');
    MonthlyDiversionFlowDialog.edtScaleFactor.OnEnter       := OnEditControlEnter;
    MonthlyDiversionFlowDialog.edtScaleFactor.OnExit        := OnEditControltExit;
    MonthlyDiversionFlowDialog.edtScaleFactor.IsEnabled     := True;

    MonthlyDiversionFlowDialog.rgrpImportIFR.FieldProperty := FAppModules.FieldProperties.FieldProperty('ImportIFR');
    MonthlyDiversionFlowDialog.rgrpImportIFR.OnEnter       := OnEditControlEnter;
    MonthlyDiversionFlowDialog.rgrpImportIFR.OnExit        := OnEditControltExit;
    MonthlyDiversionFlowDialog.rgrpImportIFR.OnClick       := OnEditControltExit;

    MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.OnBeforeCellChange := OnStringGridCellDataHasChanged;

    MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.OnSelectCell := OnstrgrdDailyInstreamFlowSelectCell;
    MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.OnTopLeftChanged := OnstrgrdDailyInstreamFlowTopLeftChanged;


    MonthlyDiversionFlowDialog.grdMonthlyCompensationValues.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    MonthlyDiversionFlowDialog.grdMonthlyCompensationValues.OnExit := OnEditControltExit;

    MonthlyDiversionFlowDialog.BtnAddDailyInstreamData.OnClick        := DoAddDailyInstreamData;
    MonthlyDiversionFlowDialog.BtnDeleteDailyInstreamData.OnClick     := DoDeleteDailyInstreamData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.OnstrgrdDailyInstreamFlowSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TMonthlyDiversionFlowValidator.OnstrgrdDailyInstreamFlowSelectCell';
begin
  try
    if ACol = 0 then
    begin
      MonthlyDiversionFlowDialog.dtpDiversionDate.Top  := 2 +MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Top +
                                                          ((1 + MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.DefaultRowHeight) *
                                                          (ARow - MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.TopRow + 1));
      MonthlyDiversionFlowDialog.dtpDiversionDate.Visible := True;
      MonthlyDiversionFlowDialog.dtpDiversionDate.IsEnabled := True;
      MonthlyDiversionFlowDialog.dtpDiversionDate.DateTime := StrtoDate(MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.cells[ACol, ARow]);
      if (MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ValidationError[ACol, ARow, gveCellContext] <> '') then
      begin
        MonthlyDiversionFlowDialog.dtpDiversionDate.ValidationError   :=
        MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ValidationError[ACol, ARow, gveCellContext];
        MonthlyDiversionFlowDialog.dtpDiversionDate.InValidationError := True;
        MonthlyDiversionFlowDialog.dtpDiversionDate.ShowErrorState(True);
      end
      else
      begin
        MonthlyDiversionFlowDialog.dtpDiversionDate.ValidationError   := '';
        MonthlyDiversionFlowDialog.dtpDiversionDate.InValidationError := False;
        MonthlyDiversionFlowDialog.dtpDiversionDate.ShowErrorState(False);
      end;

    end
    else
      MonthlyDiversionFlowDialog.dtpDiversionDate.Visible := False;
except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.OnstrgrdDailyInstreamFlowTopLeftChanged(Sender: TObject);
const OPNAME = 'TMonthlyDiversionFlowValidator.OnstrgrdDailyInstreamFlowTopLeftChanged';
begin
  try
    MonthlyDiversionFlowDialog.dtpDiversionDate.Visible := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TMonthlyDiversionFlowValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TMonthlyDiversionFlowValidator.DoContextValidation';
begin
  inherited;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TMonthlyDiversionFlowValidator.Initialise: boolean;
const OPNAME = 'TMonthlyDiversionFlowValidator.Initialise';
var
  LImportIFR : string;
  LDiversionGauge : TDiversionGauge;
begin
  Result := inherited Initialise;
  try
    ClearDataViewer;
    FDailyDataChanged := True;
    FDailyGraphDataChanged := True;
    FMonthlyGraphDataChanged := True;
    FMonthlyDataChanged := True;
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                        DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];

    LImportIFR := FAppModules.ViewIni.ReadString('TMonthlyDiversionFlowValidator','ImportIFR','');
    if LDiversionGauge <> nil then
    begin
      if LImportIFR = '1' then
        LDiversionGauge.ImportIFR := True
      else
        LDiversionGauge.ImportIFR := False;
    end;    

    MonthlyDiversionFlowDialog.dtpDiversionDate.FieldProperty := FAppModules.FieldProperties.FieldProperty('CatchmentArea');
    MonthlyDiversionFlowDialog.dtpDiversionDate.OnEnter       := OnEditControlEnter;
    MonthlyDiversionFlowDialog.dtpDiversionDate.OnExit        := OnEditControltExit;
    MonthlyDiversionFlowDialog.pgcInstreamFlow.OnChange       := OnTabChanged;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDiversionFlowValidator.LanguageHasChanged: boolean;
const OPNAME = 'TMonthlyDiversionFlowValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption :=  'Monthly Diversion Flows'; //FAppModules.Language.GetString('TabCaption.MonthlyDiversionFlow');
    Result := inherited LanguageHasChanged;
    MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[0,0] := 'IFR Date';
    MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[1,0] := 'IFR (m3/s)';
    MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[2,0] := 'Quality';
    MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[3,0] := 'Factored IFR (m3/s)';
    MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[4,0] := 'Daily Available Flow (m3/s)';
    MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[5,0] := 'Daily diversion Flow (m3/s)';

    MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[0, 0] := 'Year';
    MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[13, 0] := 'Total';

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TMonthlyDiversionFlowValidator.OnEditControltExit';
var
  LDiversionGauge: TDiversionGauge;
begin
  if(FAppModules.Model = nil) then Exit;
  inherited OnEditControltExit(Sender);
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];

    if (Sender = MonthlyDiversionFlowDialog.dtpDiversionDate) then
      UpdateDailyInstreamDataGrid;

    if (MonthlyDiversionFlowDialog.grdMonthlyCompensationValues = Sender) and (FCompensationValuesHasChanged) then
      UpdateDailyMonthlyCompensationValues;

    FCompensationValuesHasChanged := False;

    if (Sender = MonthlyDiversionFlowDialog.rgrpImportIFR) {and (MonthlyDiversionFlowDialog.rgrpImportIFR.HasValueChanged)} then
      UpdatergrpImportIFR(LDiversionGauge);
    if Sender.ClassNameIs('TFieldEdit') then
    begin
      if TFieldEdit(Sender).HasValueChanged then
      begin
        if (LDiversionGauge <> nil) then
        begin
          if (Sender = MonthlyDiversionFlowDialog.edtCapacityOfDiversion) and
          (MonthlyDiversionFlowDialog.edtCapacityOfDiversion.HasValueChanged) then
            UpdateCapacityOfDiversion(LDiversionGauge);
          if (Sender = MonthlyDiversionFlowDialog.edtScaleFactor) and
          (MonthlyDiversionFlowDialog.edtScaleFactor.HasValueChanged) then
            UpdateScaleFactor(LDiversionGauge);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.UpdatergrpImportIFR(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TMonthlyDiversionFlowValidator.UpdatergrpImportIFR';
begin
  try
    if ADiversionGauge <> nil then
    begin
      case MonthlyDiversionFlowDialog.rgrpImportIFR.ItemIndex of
        0 : ADiversionGauge.ImportIFR := False;
        1 : ADiversionGauge.ImportIFR := True;
      end;
      FAppModules.ViewIni.WriteString(ClassName,'ImportIFR',IntToStr(MonthlyDiversionFlowDialog.rgrpImportIFR.ItemIndex));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TMonthlyDiversionFlowValidator.PopulateDataViewer;
const OPNAME = 'TMonthlyDiversionFlowValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDiversionFlowValidator.RepopulateDataViewer;
const OPNAME = 'TMonthlyDiversionFlowValidator.RepopulateDataViewer';
var
  LDiversionGauge : TDiversionGauge;
  LIndex : integer;
  LOldCursor : TCursor;
begin
  try
    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LockWindowUpdate(MonthlyDiversionFlowDialog.Handle);

    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                        DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if LDiversionGauge <> nil then
    begin
      MonthlyDiversionFlowDialog.edtCapacityOfDiversion.SetFieldValue(LDiversionGauge.CapacityOfDiversion);
      MonthlyDiversionFlowDialog.edtScaleFactor.SetFieldValue(LDiversionGauge.InstreamScaleFactor);
      for LIndex := MinMonths to MaxMonths do
        MonthlyDiversionFlowDialog.grdMonthlyCompensationValues.Cells[LIndex-1, 0] := CHydroMonthsDesc[LIndex];

      for LIndex := MinMonths to MaxMonths do
      begin
        MonthlyDiversionFlowDialog.grdMonthlyCompensationValues.AddFieldProperty(
        FAppModules.FieldProperties.FieldProperty('CompensationValue'));

        MonthlyDiversionFlowDialog.grdMonthlyCompensationValues.Cells[LIndex-1,1] :=
        FloatToStr(LDiversionGauge.CompensationValueByIndex[CHydroMonths[LIndex]]);
      end;
      case IFRActiveSubTab of
        actIFRDailyGraph :  PopulateDailyInstreamGraph(LDiversionGauge);
        actIFRDailyGrid :  PopulateDailyInstreamGrid(LDiversionGauge);
        actIFRMonthlyGrid : PopulateMonthlyGridData(LDiversionGauge);
        actIFRMonthlyGraph : PopulateMonthlyGraphData(LDiversionGauge);
      end;
      if (LDiversionGauge.ImportIFR) then
      begin
        MonthlyDiversionFlowDialog.rgrpImportIFR.ItemIndex := 1;
        MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Options :=
        [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
        MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Color := clSilver;
        MonthlyDiversionFlowDialog.dtpDiversionDate.Visible := False;
        MonthlyDiversionFlowDialog.dtpDiversionDate.Enabled := False;
      end
      else
      begin
        MonthlyDiversionFlowDialog.rgrpImportIFR.ItemIndex := 0;
        MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Options :=
        [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];
        MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Color := clWindow;
        MonthlyDiversionFlowDialog.dtpDiversionDate.Enabled := True;
      end;
    end;
    if LDiversionGauge.DailyInstreamDataCount > 0 then
      MonthlyDiversionFlowDialog.rgrpImportIFR.Enabled := False
    else
      MonthlyDiversionFlowDialog.rgrpImportIFR.Enabled := True;
    LockWindowUpdate(0);
    Screen.Cursor := LOldCursor;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.PopulateDailyInstreamGraph(ADiversionGauge: TDiversionGauge);
const OPNAME = 'TMonthlyDiversionFlowValidator.PopulateDailyInstreamGraph';
var
  LIndex : integer;
  LDailyInstreamData : TDailyInstreamFlowData;
begin
  try
    if (FDailyGraphDataChanged) and (ADiversionGauge <> nil) then
    begin
      if ADiversionGauge.DailyInstreamDataCount > 0 then
      begin
        MonthlyDiversionFlowDialog.DailyInstreamLineSeries.Clear;
        for LIndex := 0 to ADiversionGauge.DailyInstreamDataCount - 1 do
        begin
          LDailyInstreamData := ADiversionGauge.InstreamFlowDataByIndex[LIndex];
          if LDailyInstreamData <> nil then
          begin
            if LDailyInstreamData.FactoredInstreamFlow <> Nullfloat then
              MonthlyDiversionFlowDialog.DailyInstreamLineSeries.AddY(LDailyInstreamData.FactoredInstreamFlow,
                                                                      DateToStr(LDailyInstreamData.InstreamDate),clTeeColor);
          end;
        end;
        FDailyGraphDataChanged := False;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.PopulateDailyInstreamGrid(ADiversionGauge: TDiversionGauge);
const OPNAME = 'TMonthlyDiversionFlowValidator.PopulateDailyInstreamGrid';
var
  LIndex : integer;
  LYear,LMonth,LDay : word;
  LDailyInstreamData : TDailyInstreamFlowData;
begin
  try
    if (FDailyDataChanged) and (ADiversionGauge <> nil) then
    begin
      if ADiversionGauge.DailyInstreamDataCount > 0 then
      begin
        MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.RowCount := ADiversionGauge.DailyInstreamDataCount + 1;
        for LIndex := 0 to ADiversionGauge.DailyInstreamDataCount - 1 do
        begin
          LDailyInstreamData := ADiversionGauge.InstreamFlowDataByIndex[LIndex];
          if LDailyInstreamData <> nil then
          begin
            DecodeDate(LDailyInstreamData.InstreamDate,LYear,LMonth,LDay);
            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.AddFieldProperty(
            FAppModules.FieldProperties.FieldProperty('InstreamDate'));
            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[0,LIndex+1] :=
            DateToStr(LDailyInstreamData.InstreamDate);

            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.AddFieldProperty(
            FAppModules.FieldProperties.FieldProperty('InstreamAvgFlow'));
            if LDailyInstreamData.AvgFlow = NullFloat then
              MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[1,LIndex+1] := ''
            else
            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[1,LIndex+1] :=
            FormatFloat('0.000',LDailyInstreamData.AvgFlow);

            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.AddFieldProperty(
            FAppModules.FieldProperties.FieldProperty('InstreamQualityCode'));
            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[2,LIndex+1] :=
            IntToStr(LDailyInstreamData.QualityCode);

            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.AddFieldProperty(
            FAppModules.FieldProperties.FieldProperty('StationNo'));
            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[3,LIndex+1] :=
            FormatFloat('0.000',LDailyInstreamData.FactoredInstreamFlow);

            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.AddFieldProperty(
            FAppModules.FieldProperties.FieldProperty('StationNo'));
            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[4,LIndex+1] :=
            FormatFloat('0.000',LDailyInstreamData.DailyAvailableFlow[LMonth]);

            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.AddFieldProperty(
            FAppModules.FieldProperties.FieldProperty('StationNo'));
            MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells[5,LIndex+1] :=
            FormatFloat('0.000',LDailyInstreamData.DailyDiversionFlow[LMonth]);
          end;
        end;
        FDailyDataChanged := False;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.PopulateMonthlyGridData(ADiversionGauge: TDiversionGauge);
const OPNAME = 'TMonthlyDiversionFlowValidator.PopulateMonthlyGridData';
var
  LMonthlyInstreamFlowData : TMonthlyInstreamFlowData;
  LIndex : integer;
  LCount : integer;
  LYearTotal : double;
  LAvarages : array[1..12] of double;
  LRecCount : array[1..12] of double;
  LGrandAvarage : double;
  LDailyDivesionProgressBar : TDailyDivesionProgressBar;
begin
  try
    if (FMonthlyDataChanged) and (ADiversionGauge <> nil) and (ADiversionGauge.MonthlyInstreamFlowCount > 0) then
    begin
      MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.RowCount := ADiversionGauge.MonthlyInstreamFlowCount + 2;
      for LCount := MinMonths to MaxMonths do
        MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[LCount,0] := CHydroMonthsDesc[LCount];
      LDailyDivesionProgressBar := TDailyDivesionProgressBar.CreateWithoutDFM(nil, FAppModules);
      LDailyDivesionProgressBar.ProgressPosition := 0;
      LDailyDivesionProgressBar.MaxProgress := ADiversionGauge.MonthlyInstreamFlowCount;
      LDailyDivesionProgressBar.Initialise;
      LDailyDivesionProgressBar.ShowForm;
      try
        LGrandAvarage := 0;
        for LIndex := 0 to ADiversionGauge.MonthlyInstreamFlowCount -1 do
        begin
          LDailyDivesionProgressBar.ProgressPosition := LDailyDivesionProgressBar.ProgressPosition + 1;
          LMonthlyInstreamFlowData := ADiversionGauge.MonthlyInstreamFlowDataByIndex[LIndex];
          if (LMonthlyInstreamFlowData <> nil) then
          begin
            LYearTotal := 0;
            for LCount := MinMonths to MaxMonths do
            begin
              MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.ColWidths[0] := 95;
              MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.ColWidths[LCount] := 60;
              MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[0,LIndex+1] :=
              IntToStr(LMonthlyInstreamFlowData.Year)+'/'+IntToStr(LMonthlyInstreamFlowData.Year+1);
              if LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount] = NullFloat then
              MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[LCount,LIndex+1] := ''
              else
              begin
                MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[LCount,LIndex+1] :=
                FormatFloat('0.000',LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount]);
                LYearTotal := LYearTotal + LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount];
                LAvarages[LCount] := LAvarages[LCount] + LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount];
                LRecCount[LCount] := LRecCount[LCount]+1;
              end;
            end;
             LGrandAvarage := LGrandAvarage + LYearTotal;
            MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[13,LIndex+1] := FormatFloat('0.000',LYearTotal);
          end;
        end;
        for LCount := Low(LAvarages) to High(LAvarages) do
        begin
          LAvarages[LCount] := LAvarages[LCount]/LRecCount[LCount];
          MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[LCount,MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.RowCount-1] := FormatFloat('####0.000',LAvarages[LCount]);
          MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[0,MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.RowCount-1] := 'Average';
        end;
        LGrandAvarage := LGrandAvarage/ADiversionGauge.MonthlyDailyFlowCount;
        MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.Cells[13,MonthlyDiversionFlowDialog.strgrdMonthlyInstreamFlow.RowCount-1] := FormatFloat('####0.000',LGrandAvarage);
        FMonthlyDataChanged := False;
      finally
        FreeAndNil(LDailyDivesionProgressBar)
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.PopulateMonthlyGraphData(ADiversionGauge: TDiversionGauge);
const OPNAME = 'TMonthlyDiversionFlowValidator.PopulateMonthlyGraphData';
var
  LMonthlyInstreamFlowData : TMonthlyInstreamFlowData;
  LIndex : integer;
  LCount : integer;
  LDayLabel : string;
  LDailyDivesionProgressBar : TDailyDivesionProgressBar;
begin
  try
    if (FMonthlyGraphDataChanged) and (ADiversionGauge <> nil) and (ADiversionGauge.MonthlyInstreamFlowCount > 0) then
    begin
      MonthlyDiversionFlowDialog.MonthlyInstreamLineSeries.Clear;

      LDailyDivesionProgressBar := TDailyDivesionProgressBar.CreateWithoutDFM(nil, FAppModules);
      try
        LDailyDivesionProgressBar.ProgressPosition := 0;
        LDailyDivesionProgressBar.MaxProgress := ADiversionGauge.MonthlyInstreamFlowCount;
        LDailyDivesionProgressBar.Initialise;
        LDailyDivesionProgressBar.ShowForm;

        for LIndex := 0 to ADiversionGauge.MonthlyInstreamFlowCount -1 do
        begin
          LDailyDivesionProgressBar.ProgressPosition := LDailyDivesionProgressBar.ProgressPosition + 1;

          LMonthlyInstreamFlowData := ADiversionGauge.MonthlyInstreamFlowDataByIndex[LIndex];
          if (LMonthlyInstreamFlowData <> nil) then
          begin
            for LCount := MinMonths to MaxMonths do
            begin
              if LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount] <> NullFloat then
              begin
                LDayLabel := FormatDateTime('yyyy/mm',(EncodeDate(LMonthlyInstreamFlowData.Year,LCount,01)));
                MonthlyDiversionFlowDialog.MonthlyInstreamLineSeries.AddY(LMonthlyInstreamFlowData.DailyDiversionFlowByIndex[LCount],
                                                                LDayLabel, clTeeColor);
              end;
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

function TMonthlyDiversionFlowValidator.GetMonthlyDiversionFlowDialog : TMonthlyDiversionFlowDialog;
const OPNAME = 'TMonthlyDiversionFlowValidator.GetMonthlyDiversionFlowDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TMonthlyDiversionFlowDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMonthlyDiversionFlowValidator.SaveState: boolean;
const OPNAME = 'TMonthlyDiversionFlowValidator.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TMonthlyDiversionFlowValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                      ANewValue: string): boolean;
const OPNAME = 'TMonthlyDiversionFlowValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'DiversionDate') or (AFieldName = 'AvgFlow') or (AFieldName = 'InstreamAvgFlow')
       or (AFieldName = 'QualityCode') or (AFieldName = 'InstreamScaleFactor')
       or (AFieldName = 'CatchmentScaleFactor')or (AFieldName = 'CapacityOfDiversion')
       or (AFieldName = 'CompensationValue') or (AFieldName = 'StationNo') then
    begin
      FDailyDataChanged := True;
      FDailyGraphDataChanged := True;
      FMonthlyGraphDataChanged := True;
      FMonthlyDataChanged := True;
      if (AFieldName <> 'CompensationValue') then
        PopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDiversionFlowValidator.StudyHasChanged: boolean;
const OPNAME = 'TMonthlyDiversionFlowValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TMonthlyDiversionFlowValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMonthlyDiversionFlowValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    if (MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow = ASender) then
      UpdateDailyInstreamDataGrid;
    if (MonthlyDiversionFlowDialog.grdMonthlyCompensationValues = ASender) then
      FCompensationValuesHasChanged := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDiversionFlowValidator.UpdateDailyMonthlyCompensationValues;
const OPNAME = 'TMonthlyDiversionFlowValidator.UpdateDailyMonthlyCompensationValues';
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
        LValue := MonthlyDiversionFlowDialog.grdMonthlyCompensationValues.Cells[LCol-1,1];
        MonthlyDiversionFlowDialog.grdMonthlyCompensationValues.ValidationError[LCol,1, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty('CompensationValue', LValue,LMessage,LCol)) then
        begin
          LDiversionGauge.CompensationValueByIndex[LCol] := StrToFloat(LValue);
        end
        else
          MonthlyDiversionFlowDialog.grdMonthlyCompensationValues.ValidationError[LCol,1, gveCellContext] := LMessage;
      end;
      PopulateDataViewer;
      DoContextValidation(dvtCompensationValue);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.UpdateCapacityOfDiversion(ADiversionGauge: TDiversionGauge);
const OPNAME = 'TMonthlyDiversionFlowValidator.UpdateCapacityOfDiversion';
var
  LMessage    : string;
  LFieldValue : string;
begin
  try
    if (ADiversionGauge <> nil) then
    begin
      MonthlyDiversionFlowDialog.edtCapacityOfDiversion.FieldValidationError :='';
      LFieldValue := MonthlyDiversionFlowDialog.edtCapacityOfDiversion.Text;
      if (FAppModules.FieldProperties.ValidateFieldProperty('CapacityOfDiversion',
          LFieldValue, LMessage)) then
      begin
        ADiversionGauge.CapacityOfDiversion := StrToFloat(LFieldValue);
        PopulateDataViewer;
        DoContextValidation(dvtCapacityOfDiversion);
      end
      else
        MonthlyDiversionFlowDialog.edtCapacityOfDiversion.FieldValidationError := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.UpdateScaleFactor(ADiversionGauge: TDiversionGauge);
const OPNAME = 'TMonthlyDiversionFlowValidator.UpdateScaleFactor';
var
  LMessage    : string;
  LFieldValue : string;
begin
  try
    if (ADiversionGauge <> nil) then
    begin
      MonthlyDiversionFlowDialog.edtScaleFactor.FieldValidationError :='';
      LFieldValue := MonthlyDiversionFlowDialog.edtScaleFactor.Text;
      if (FAppModules.FieldProperties.ValidateFieldProperty('InstreamScaleFactor',
          LFieldValue, LMessage)) then
      begin
        ADiversionGauge.InstreamScaleFactor := StrToFloat(LFieldValue);
        if ADiversionGauge.WRYMDataCount > 0 then
          TDailyDiversionDataObject(FAppModules.Model.ModelData).GenerateFlowDiversionRelation(ADiversionGauge.StationID);
        PopulateDataViewer;
        DoContextValidation(dvtScaleFactor);
      end
      else
        MonthlyDiversionFlowDialog.edtScaleFactor.FieldValidationError := LMessage;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TMonthlyDiversionFlowValidator.DoAddDailyInstreamData(Sender: TObject);
const OPNAME = 'TMonthlyDiversionFlowValidator.DoAddDailyInstreamData';
var
  LDiversionGauge : TDiversionGauge;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if LDiversionGauge <> nil then
    begin
      LDailyInstreamFlowData := LDiversionGauge.NewDailyInstreamFlowData(Now);
      if LDailyInstreamFlowData <> nil then
      begin
        PopulateDataViewer;
        DoContextValidation(dvtMonthlyReference);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.DoDeleteDailyInstreamData(Sender: TObject);
const OPNAME = 'TMonthlyDiversionFlowValidator.DoDeleteDailyInstreamData';
var
  LDiversionGauge : TDiversionGauge;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if (LDiversionGauge <> nil) and (LDiversionGauge.DailyInstreamDataCount > 0) then
    begin

      LDailyInstreamFlowData := LDiversionGauge.InstreamFlowDataByIndex[
                                MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Row-1];
      if LDailyInstreamFlowData <> nil then
        if LDiversionGauge.RemoveDailyInstreamData(LDailyInstreamFlowData.Identifier) then
        begin
          PopulateDataViewer;
          DoContextValidation(dvtMonthlyReference);
        end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TMonthlyDiversionFlowValidator.UpdateDailyInstreamDataGrid;
const OPNAME = 'TMonthlyDiversionFlowValidator.UpdateDailyInstreamDataGrid';
var
  LMessage : string;
  LDiversionGauge : TDiversionGauge;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
  LValue : string;
  LRow : integer;
  LInstreamDate : TDateTime;
begin
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    LValue := MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Cells
              [MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Col,
               MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Row];
    LRow := MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Row;

    LDailyInstreamFlowData := LDiversionGauge.InstreamFlowDataByIndex[LRow-1];
    if (LDiversionGauge <> nil) and not (LDiversionGauge.ImportIFR) and (LDailyInstreamFlowData <> nil) then
    begin
      if (MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Col = 0 ) and
       (MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Row > 0) then
      begin
        LInstreamDate := 0;
        if (MonthlyDiversionFlowDialog.dtpDiversionDate.DateTime > 0) then
          LInstreamDate := MonthlyDiversionFlowDialog.dtpDiversionDate.DateTime;
        if LInstreamDate <= 0 then
          Exit;

        MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ValidationError[0, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'InstreamDate', DateToStr(LInstreamDate),LMessage)) then
        begin
          LDailyInstreamFlowData.InstreamDate := LInstreamDate;
          PopulateDataViewer;
          DoContextValidation(dvtInstreamDate);
        end
        else
          MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ValidationError[0, LRow, gveCellContext] := LMessage;
      end;

      if (MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Col = 1 ) and
       (MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Row > 0) then
      begin
        MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ValidationError[1, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'InstreamAvgFlow', LValue,LMessage)) then
        begin
          LDailyInstreamFlowData.AvgFlow := StrToFloat(LValue);
          PopulateDataViewer;
          DoContextValidation(dvtAvgFlow);
        end
        else
          MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ValidationError[1, LRow, gveCellContext] := LMessage;
      end;

      if (MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Col = 2 ) and
       (MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.Row > 0) then
      begin
        MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ValidationError[1, LRow, gveCellContext] := '';
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'InstreamQualityCode', LValue,LMessage)) then
        begin
          LDailyInstreamFlowData.QualityCode := StrToInt(LValue);
          PopulateDataViewer;
          DoContextValidation(dvtQualityCode);
        end
        else
          MonthlyDiversionFlowDialog.strgrdDailyInstreamFlow.ValidationError[1, LRow, gveCellContext] := LMessage;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TMonthlyDiversionFlowValidator.CanExport: boolean;
const OPNAME = 'TMonthlyDiversionFlowValidator.CanExport';
begin
  Result := False;
  try
    if(MonthlyDiversionFlowDialog <> nil) then
      Result := MonthlyDiversionFlowDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMonthlyDiversionFlowValidator.CanPrint: boolean;
const OPNAME = 'TMonthlyDiversionFlowValidator.CanPrint';
begin
  Result := False;
  try
    if(MonthlyDiversionFlowDialog <> nil) then
      Result := MonthlyDiversionFlowDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDiversionFlowValidator.DoExport(AFileName: string);
const OPNAME = 'TMonthlyDiversionFlowValidator.DoExport';
begin
  try
    if(MonthlyDiversionFlowDialog <> nil) then
      MonthlyDiversionFlowDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMonthlyDiversionFlowValidator.DoPrint;
const OPNAME = 'TMonthlyDiversionFlowValidator.DoPrint';
begin
  try
    if(MonthlyDiversionFlowDialog <> nil) then
      MonthlyDiversionFlowDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
