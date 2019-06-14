//
//
//  UNIT      : Contains TDailyIFRDataValidator Class
//  AUTHOR    : Sam Dhlamini(ARIVIA)
//  DATE      : 29/11/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//


unit UDailyIFRDataValidator;

interface
uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  //Chart,
  //Series,
  VCL.Graphics,
  VCL.Dialogs,
  //TeeProcs,
  //TeEngine,
  Math,
  VCL.Forms,
  Windows,
  Contnrs,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UDataComponent,
  UDailyIFRDataDialog,
  UDailyIFRData,
  UIFRFeatures,
  UProgressDialog,
  UDailyDiversionGaugeData,
  UDailyDiversionContextValidationType,
  UDailyDiversionMenuItemManager,
  UDailyDiversionModelManager,
  UGenericModelLinkClasses;
type
  TDailyIFRDataValidator = class (TAbstractDataDialogValidator)
  protected
    FMenuItemManager : TMenuItemManager;
    FProgressDialog  : TProgressDialog;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure DoOncbxChannelSelect(Sender : TObject);
    procedure UpdateScaleFactor(ADiversionGauge : TDiversionGauge);

    procedure OnStringGridCellDataHasChanged(ASender: TObject;ACol: Integer; ARow: Integer); override;
    procedure OnTabChanged(Sender: Tobject);
    procedure PopulateMonthlyIFRFeatureGraph(AIFRFeature : TIFRFeature;AMonth : integer;AFactor : double);
    procedure PopulateDailyIFRForAllMonths;
    procedure GenerateDailyIFR(Sender : TObject);
    function ExecGenerateDailyIFR(AProgressUpdateFuntion : TProgressUpdateFuntion) : boolean;
    function GenerateDailyIFRByMonth(AMonth : integer) : boolean;
    function GetMonthlyIFRFactor(AIFRFeature : TIFRFeature; AFactor,AMonthValue : Double;AMonth : integer) : double;
    procedure RepopulateDataViewer;
    procedure OnItemCheckUncheck(Sender: TObject);

    function GetDailyIFRDataDialog : TDailyIFRDataDialog;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation(AValidationType: TDialogValidationType);
    property DailyIFRDataDialog : TDailyIFRDataDialog read GetDailyIFRDataDialog;
end;

implementation
uses
  SysUtils,
  UConstants,
  UDataEditComponent,
//  UDailyDivesionProgressBar,

  UDailyDiversionDataObject,
  UErrorHandlingOperations, VCL.Grids, UChannelData;
{ DailyDiversionValidator }


procedure TDailyIFRDataValidator.OnTabChanged(Sender: Tobject);
const OPNAME = 'TDailyIFRDataValidator.OnTabChanged';
begin
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDailyIFRDataValidator.ClearDataViewer;
const OPNAME = 'TDailyIFRDataValidator.ClearDataViewer';
var
  LRowIndex,
  LColIndex : integer;
begin
  inherited ClearDataViewer;
  try
    DailyIFRDataDialog.cbxChannel.Clear;
    for LRowIndex := 1 to DailyIFRDataDialog.MonthlyIFRGrid.RowCount -1 do
    begin
      for LColIndex := 1 to  DailyIFRDataDialog.MonthlyIFRGrid.ColCount -1 do
        DailyIFRDataDialog.MonthlyIFRGrid.Cells[LColIndex,LRowIndex] := ''
    end;

    for LRowIndex := 1 to DailyIFRDataDialog.DailyIFRGrid.RowCount -1 do
    begin
      for LColIndex := 0 to  DailyIFRDataDialog.DailyIFRGrid.ColCount -1 do
        DailyIFRDataDialog.DailyIFRGrid.Cells[LColIndex,LRowIndex] := ''
    end;

    DailyIFRDataDialog.MonthlyIFRGrid.RowCount := 2;
    DailyIFRDataDialog.MonthlyIFRGrid.ColCount := 3;
    DailyIFRDataDialog.DailyIFRGrid.RowCount := 2;
    DailyIFRDataDialog.DailyIFRGrid.ColCount := 2;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyIFRDataValidator.CreateMemberObjects;
const OPNAME = 'TDailyIFRDataValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TDailyIFRDataDialog.Create(nil, FAppModules);
    FMenuItemManager := TMenuItemManager.Create(FAppModules);
    FProgressDialog := TProgressDialog.Create(nil,FAppModules);

    DailyIFRDataDialog.cbxChannel.FieldProperty    := FAppModules.FieldProperties.FieldProperty('ChannelName');
    DailyIFRDataDialog.cbxChannel.OnEnter          := OnEditControlEnter;
    DailyIFRDataDialog.cbxChannel.OnExit           := OnEditControltExit;
    DailyIFRDataDialog.cbxChannel.Enabled          := True;
    DailyIFRDataDialog.cbxChannel.Sorted           := True;
    DailyIFRDataDialog.cbxChannel.OnSelect         := DoOncbxChannelSelect;

    DailyIFRDataDialog.cbxCurrentMonth.FieldProperty    := FAppModules.FieldProperties.FieldProperty('Month');
    DailyIFRDataDialog.cbxCurrentMonth.OnEnter          := OnEditControlEnter;
    DailyIFRDataDialog.cbxCurrentMonth.OnExit           := OnEditControltExit;
    DailyIFRDataDialog.cbxCurrentMonth.Enabled          := True;
    DailyIFRDataDialog.cbxCurrentMonth.OnSelect         := DoOncbxChannelSelect;

    DailyIFRDataDialog.btnGenerateDailyIFR.OnClick      := GenerateDailyIFR; //DobtnGenerateDailyIFRClick;

    DailyIFRDataDialog.edtScaleFactor.FieldProperty := FAppModules.FieldProperties.FieldProperty('InstreamScaleFactor');
    DailyIFRDataDialog.edtScaleFactor.OnEnter       := OnEditControlEnter;
    DailyIFRDataDialog.edtScaleFactor.OnExit        := OnEditControltExit;
    DailyIFRDataDialog.edtScaleFactor.IsEnabled     := True;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TDailyIFRDataValidator.DestroyMemberObjects;
const OPNAME = 'TDailyIFRDataValidator.DestroyMemberObjects';
begin
  try
    FreeAndNil(FMenuItemManager);
    FreeAndNil(FProgressDialog);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TDailyIFRDataValidator.DoContextValidation';
begin
  inherited;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDailyIFRDataValidator.Initialise: boolean;
const OPNAME = 'TDailyIFRDataValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ClearDataViewer;
    DailyIFRDataDialog.btnGenerateDailyIFR.Enabled := (FAppModules.User.UserRights in CUR_EditData) and
                                                      not(FAppModules.StudyArea.ScenarioLocked);
    FProgressDialog.clbOptions.OnClickCheck := OnItemCheckUncheck;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyIFRDataValidator.OnItemCheckUncheck(Sender: TObject);
const OPNAME = 'TDailyIFRDataValidator.OnItemCheckUncheck';
begin
  try
    FAppModules.GlobalData.SetStopOnFirstErr(FProgressDialog.clbOptions.Checked[0]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyIFRDataValidator.LanguageHasChanged: boolean;
const OPNAME = 'TDailyIFRDataValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := inherited LanguageHasChanged;
    TabShetCaption :=  FAppModules.Language.GetString('TabCaption.CreateDailyIFRData');
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TDailyIFRDataValidator.OnEditControltExit';
var
  LDiversionGauge : TDiversionGauge;
begin
  inherited OnEditControltExit(Sender);
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    if (Sender = DailyIFRDataDialog.edtScaleFactor) AND
       (DailyIFRDataDialog.edtScaleFactor.HasValueChanged) then
    begin
      DoOncbxChannelSelect(Sender);
      if not (DailyIFRDataDialog.edtScaleFactor.ReadOnly) and (LDiversionGauge <> nil) and
        (DailyIFRDataDialog.edtScaleFactor.HasValueChanged) then
      begin
        UpdateScaleFactor(LDiversionGauge);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataValidator.UpdateScaleFactor(ADiversionGauge : TDiversionGauge);
const OPNAME = 'TDailyIFRDataValidator.UpdateScaleFactor';
var
  LMessage    : string;
  LFieldValue : string;
begin
  try
    if (ADiversionGauge <> nil) and (DailyIFRDataDialog.btnGenerateDailyIFR.Enabled) then
    begin
      LFieldValue := DailyIFRDataDialog.edtScaleFactor.Text;
      if(LFieldValue <> '') then
      begin
        FAppModules.ViewIni.WriteString(ClassName,'CatchmentScaleFactor',LFieldValue);
        DailyIFRDataDialog.edtScaleFactor.FieldValidationError :='';
        if ADiversionGauge.WRYMDataCount > 0 then
          TDailyDiversionDataObject(FAppModules.Model.ModelData).GenerateFlowDiversionRelation(ADiversionGauge.StationID);
      end
      else
        DailyIFRDataDialog.edtScaleFactor.FieldValidationError := LMessage;
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TDailyIFRDataValidator.DoOncbxChannelSelect(Sender : TObject);
const OPNAME = 'TDailyIFRDataValidator.DoOncbxChannelSelect';
var
  LChannelNumber : integer;
  LIFRFeature : TIFRFeature;
  LDailyIFRData : TDailyIFRData;
  LIndex : integer;
  LMonth : integer;
  LFactor : double;
  LOldCursor : TCursor;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
  LDiversionGauge : TDiversionGauge;

begin
  try
    LMonth := -1;
    LChannelNumber := 0;
    DailyIFRDataDialog.MonthlyIFRSeries.Clear;
    DailyIFRDataDialog.MonthlyFlowIFRSeries.Clear;
    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LockWindowUpdate(DailyIFRDataDialog.Handle);
    try
      LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];

      for LIndex := 1 to DailyIFRDataDialog.DailyIFRGrid.RowCount -1 do
      begin
        DailyIFRDataDialog.DailyIFRGrid.Cells[0,LIndex] := '';
        DailyIFRDataDialog.DailyIFRGrid.Cells[1,LIndex] := '';
      end;

      DailyIFRDataDialog.DailyIFRGrid.RowCount := 2;
      if DailyIFRDataDialog.cbxChannel.Text <> '' then
        DailyIFRDataDialog.cbxChannel.ItemIndex := DailyIFRDataDialog.cbxChannel.Items.IndexOf(
                                                   DailyIFRDataDialog.cbxChannel.Text);
      if DailyIFRDataDialog.cbxChannel.ItemIndex <> -1 then
        LChannelNumber := Integer(DailyIFRDataDialog.cbxChannel.Items.Objects[DailyIFRDataDialog.cbxChannel.ItemIndex]);

      LDailyIFRData := nil;
      if LDiversionGauge <> nil then
        LDailyIFRData := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRData[LDiversionGauge.StationNo];

      LIFRFeature := LDailyIFRData.IFRFeatureList.CastMonthlyIFRFeatureByNumber(LChannelNumber);
      if DailyIFRDataDialog.cbxCurrentMonth.Text <> '' then
        DailyIFRDataDialog.cbxCurrentMonth.ItemIndex := DailyIFRDataDialog.cbxCurrentMonth.Items.IndexOf(
                                                        DailyIFRDataDialog.cbxCurrentMonth.Text)
      else
      if DailyIFRDataDialog.cbxCurrentMonth.Text = '' then
        DailyIFRDataDialog.cbxCurrentMonth.ItemIndex := -1;

      if DailyIFRDataDialog.cbxCurrentMonth.ItemIndex > -1 then
        LMonth := Integer(DailyIFRDataDialog.cbxCurrentMonth.Items.Objects[DailyIFRDataDialog.cbxCurrentMonth.ItemIndex]);
      if LMonth = -1 then Exit;
      if LIFRFeature <> nil then
      begin
        DailyIFRDataDialog.MonthlyIFRGrid.RowCount := 13;
        LFactor := 0;
        if (DailyIFRDataDialog.edtScaleFactor.Text <> '') and (StrToFloat(DailyIFRDataDialog.edtScaleFactor.Text) > 0) then
        begin
          DailyIFRDataDialog.MonthlyIFRGrid.ColCount := 5;
          DailyIFRDataDialog.MonthlyIFRGrid.Cells [3,0] := FAppModules.Language.GetString('GridHeading.FactoredIFR');
          DailyIFRDataDialog.MonthlyIFRGrid.Cells [4,0] := FAppModules.Language.GetString('GridHeading.FactoredFlow');
          LFactor := StrToFloat(DailyIFRDataDialog.edtScaleFactor.Text);
        end
        else
          DailyIFRDataDialog.MonthlyIFRGrid.ColCount := 3;
        for LIndex := 1 to 12 do
        begin
          DailyIFRDataDialog.MonthlyIFRGrid.Cells[0,LIndex] := IntToStr(LIndex);
          if LIFRFeature.InflowByIndexAndMonth[LIndex,LMonth] = NullFloat then
            DailyIFRDataDialog.MonthlyIFRGrid.Cells[1,LIndex] := ''
          else
          begin
            if (DailyIFRDataDialog.edtScaleFactor.Text <> '') and (StrToFloat(DailyIFRDataDialog.edtScaleFactor.Text) > 0) then
            begin
              DailyIFRDataDialog.MonthlyIFRGrid.Cells[3,LIndex] := (FloatToStr(LIFRFeature.InflowByIndexAndMonth[LIndex,LMonth]*LFactor));
              DailyIFRDataDialog.MonthlyIFRGrid.Cells[1,LIndex] := FloatToStr(LIFRFeature.InflowByIndexAndMonth[LIndex,LMonth]);
            end
            else
              DailyIFRDataDialog.MonthlyIFRGrid.Cells[1,LIndex] := FloatToStr(LIFRFeature.InflowByIndexAndMonth[LIndex,LMonth]);
          end;
          if LIFRFeature.ReleaseByIndexAndMonth[LIndex,LMonth] = NullFloat then
            DailyIFRDataDialog.MonthlyIFRGrid.Cells[2,LIndex] := ''
          else
          begin
            if (DailyIFRDataDialog.edtScaleFactor.Text <> '') and (StrToFloat(DailyIFRDataDialog.edtScaleFactor.Text) > 0) then
            begin
              DailyIFRDataDialog.MonthlyIFRGrid.Cells[4,LIndex] := (FloatToStr(LIFRFeature.ReleaseByIndexAndMonth[LIndex,LMonth]*LFactor));
              DailyIFRDataDialog.MonthlyIFRGrid.Cells[2,LIndex] := FloatToStr(LIFRFeature.ReleaseByIndexAndMonth[LIndex,LMonth]);
            end
            else
              DailyIFRDataDialog.MonthlyIFRGrid.Cells[2,LIndex] := FloatToStr(LIFRFeature.ReleaseByIndexAndMonth[LIndex,LMonth]);
          end;
        end;
        PopulateMonthlyIFRFeatureGraph(LIFRFeature,LMonth,LFactor);
        if LMonth > 0 then
        begin
          if (GenerateDailyIFRByMonth(LMonth)) and (LDailyIFRData.DailyIFRList.Count>0) then
          begin
            DailyIFRDataDialog.DailyIFRGrid.RowCount := LDailyIFRData.DailyIFRList.Count + 1;
            for LIndex := 0 to LDailyIFRData.DailyIFRList.Count-1 do
            begin
              LDailyInstreamFlowData := TDailyInstreamFlowData(LDailyIFRData.DailyIFRList.Objects[LIndex]);
              if LDailyInstreamFlowData <> nil then
              begin
                DailyIFRDataDialog.DailyIFRGrid.Cells[0,LIndex+1] := DateToStr(LDailyInstreamFlowData.InstreamDate);
                if LDailyInstreamFlowData.AvgFlow <> NullFloat then
                  DailyIFRDataDialog.DailyIFRGrid.Cells[1,LIndex+1] := FloatToStr(LDailyInstreamFlowData.AvgFlow);
                DailyIFRDataDialog.DailyIFRGrid.RowHeights[LIndex+1] := 15;
              end;
            end;
          end;
        end
        else
        if LMonth = 0 then
          PopulateDailyIFRForAllMonths;
      end;
//      else
//        ClearDataViewer;
    finally
      LockWindowUpdate(0);
      Screen.Cursor := LOldCursor;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataValidator.PopulateDataViewer;
const OPNAME = 'TDailyIFRDataValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyIFRDataValidator.RepopulateDataViewer;
const OPNAME = 'TDailyIFRDataValidator.RepopulateDataViewer';
var
  LDailyIFRData : TDailyIFRData;
  LIndex : integer;
begin
  try
    LDailyIFRData := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRData[TDailyDiversionDataObject(FAppModules.Model.ModelData).
          DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier].StationNo];
    if LDailyIFRData <> nil then
    begin
      if (TDailyDiversionDataObject(FAppModules.Model.ModelData).
          DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier].
          DailyInstreamDataCount > 0) then
      begin
        DailyIFRDataDialog.edtScaleFactor.ReadOnly := True;
        DailyIFRDataDialog.edtScaleFactor.Color := clSilver;
      end
      else
      begin
        DailyIFRDataDialog.edtScaleFactor.ReadOnly := False;
        DailyIFRDataDialog.edtScaleFactor.Color := clWindow;
      end;
      DailyIFRDataDialog.edtScaleFactor.SetFieldValue(FAppModules.ViewIni.ReadString(ClassName,'CatchmentScaleFactor','1.0000'));
      DailyIFRDataDialog.btnGenerateDailyIFR.Enabled := (TDailyDiversionDataObject(FAppModules.Model.ModelData).
                                                         DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier].
                                                         DailyInstreamDataCount = 0);
      if LDailyIFRData.IFRFeatureList.MonthlyIFRFeatureCount > 0 then
      begin
        if LDailyIFRData.ChannelList <> nil then
        begin
          for LIndex := 0 to 12 do
          begin
            if LIndex = 0 then
            begin
              DailyIFRDataDialog.cbxCurrentMonth.Items.AddObject('ALL',TObject(LIndex));
              Continue;
            end;
            DailyIFRDataDialog.cbxCurrentMonth.Items.AddObject(CHydroMonthsDesc[LIndex],TObject(LIndex));
          end;
          for LIndex := 0 to LDailyIFRData.ChannelList.ChannelCount -1 do
            DailyIFRDataDialog.cbxChannel.Items.AddObject(LDailyIFRData.ChannelList.ChannelByIndex[LIndex].ChannelName,
            TObject(LDailyIFRData.ChannelList.ChannelByIndex[LIndex].ChannelNumber));
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataValidator.PopulateMonthlyIFRFeatureGraph(AIFRFeature : TIFRFeature;AMonth : integer; AFactor : Double);
const OPNAME = 'TDailyIFRDataValidator.PopulateMonthlyIFRFeatureGraph';
  CExcedence : array [1..12]of integer = (100,99,90,80,70,60,50,40,30,20,10,0);
var
  LIndex : integer;
begin
  try
    if AIFRFeature <> nil then
    begin
      DailyIFRDataDialog.MonthlyIFRGraph.Legend.Visible := True;
      DailyIFRDataDialog.MonthlyIFRGraph.Series[0].Title := 'Reference Flow';
      DailyIFRDataDialog.MonthlyIFRGraph.Series[1].Title := 'IFR';
      DailyIFRDataDialog.MonthlyIFRGraph.BottomAxis.SetMinMax(0,12);
      DailyIFRDataDialog.MonthlyIFRGraph.LeftAxis.SetMinMax(0,10);

      for LIndex := 12 downto 1 do
      begin
        if (AIFRFeature.InflowByIndexAndMonth[LIndex,AMonth] <> NullFloat) then
          DailyIFRDataDialog.MonthlyIFRSeries.AddY(((AIFRFeature.InflowByIndexAndMonth[LIndex,AMonth])*AFactor));
        if (AIFRFeature.InflowByIndexAndMonth[LIndex,AMonth] <> NullFloat) and
           (AIFRFeature.ReleaseByIndexAndMonth[LIndex,AMonth] <> NullFloat)then
          DailyIFRDataDialog.MonthlyFlowIFRSeries.AddY((AIFRFeature.ReleaseByIndexAndMonth[LIndex,AMonth]*AFactor),InttoStr(CExcedence[LIndex]));
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRDataValidator.GenerateDailyIFRByMonth(AMonth : integer) : boolean;
const OPNAME = 'TDailyIFRDataValidator.GenerateDailyIFRByMonth';
var
  LDiversionGauge : TDiversionGauge;
  LDailyFlowDataList : TStringList;
  LCalculatedIFRList : TStringList;
  LSortedIFRList : TStringList;
  LDailyIFRData : TDailyIFRData;
  LDailyFlowData : TDailyFlowData;
  LCalcFactor : double;
  LIFRFeature : TIFRFeature;
  LIndex : integer;
  LMonthlyFlowData : TMonthlyFlowData;
  LPreYear : integer;
  LMonthValue : double;
  LMonth : word;
  LDay : word;
  LYear : word;
  LFactor : double;
  LDailyAvgFlow : double;
  LChannelNumber : integer;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
  LHydroMonth : integer;
begin
  Result := False;
  try
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    LDailyIFRData := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRData[LDiversionGauge.StationNo];
    LChannelNumber := 0;
    if DailyIFRDataDialog.cbxChannel.ItemIndex <> -1 then
      LChannelNumber := Integer(DailyIFRDataDialog.cbxChannel.Items.Objects[DailyIFRDataDialog.cbxChannel.ItemIndex]);
    LIFRFeature := LDailyIFRData.IFRFeatureList.CastMonthlyIFRFeatureByNumber(LChannelNumber);
    if (LDiversionGauge <> nil) and (LIFRFeature <> nil)then
    begin
      LDailyFlowDataList := TStringList.Create;
      LCalculatedIFRList := TStringList.Create;
      LCalculatedIFRList.Sorted := True;
      LSortedIFRList := TStringList.Create;
      try
        LDailyFlowDataList.Clear;
        LMonth := CHydroMonths[AMonth];
        LDiversionGauge.GetDailyDiversionDataByMonth(LMonth,LDailyFlowDataList);
        if (LDailyFlowDataList.Count > 0) then
        begin
          LPreYear := 0;
          LMonthValue := 0;
          LHydroMonth := 0;
          LFactor := StrToFloat(DailyIFRDataDialog.edtScaleFactor.Text);
          for LIndex := 0 to LDailyFlowDataList.Count-1 do
          begin
            LDailyFlowData := TDailyFlowData(LDailyFlowDataList.Objects[LIndex]);
            if LDailyFlowData <> nil then
            begin
              DecodeDate(LDailyFlowData.DiversionDate,LYear,LMonth,LDay);
              LDailyAvgFlow := NullFloat;
              if LMonth < 10 then
                LYear := LYear-1;
              if LYear <>  LPreYear then
              begin
                LMonthlyFlowData := LDiversionGauge.MonthlyFlowDataByYear[LYear];
                LPreYear := LYear;
                if LMonthlyFlowData <> nil then
                begin
                  if LMonth > 9 then
                    LHydroMonth := LMonth - 9
                  else
                    LHydroMonth := LMonth + 3;
                  LMonthValue := LMonthlyFlowData.AvgFlowByIndex[AMonth];
                end;
              end;
              LCalcFactor := GetMonthlyIFRFactor(LIFRFeature,LFactor,LMonthValue,LHydroMonth);
              if (LCalcFactor > 0) and (LDailyFlowData.FactoredFlow <> NullFloat) then
                LDailyAvgFlow := StrToFloat(FormatFloat('##0.000',LDailyFlowData.FactoredFlow*LCalcFactor));
              LDailyInstreamFlowData := LDiversionGauge.AddInstreamFlowData(LDailyFlowData.DiversionDate);
              if LDailyInstreamFlowData <> nil then
              begin
                LDailyInstreamFlowData.Poulate(LDailyFlowData.StationID,LDailyFlowData.Identifier,
                                      LDailyAvgFlow,LDiversionGauge.InstreamScaleFactor,
                                      LDiversionGauge.CompensationValueByIndex[LMonth],
                                      LDiversionGauge.CapacityOfDiversion,LDailyFlowData.QualityCode);
                LCalculatedIFRList.AddObject(FloatToStr(LDailyFlowData.DiversionDate),LDailyInstreamFlowData);
              end;
            end;
          end;
        end;
        for LIndex := 0 to LCalculatedIFRList.Count-1 do
        begin
          LDailyInstreamFlowData := TDailyInstreamFlowData(LCalculatedIFRList.Objects[LIndex]);
          LSortedIFRList.AddObject(formatDateTime('yyyy/mm/dd',LDailyInstreamFlowData.InstreamDate),LDailyInstreamFlowData);
        end;
        LDailyIFRData.DailyIFRList.Assign(LSortedIFRList);
        LDiversionGauge.ClearDailyInstreamFlowFileData;
      finally
        LDailyFlowDataList.Free;
        LCalculatedIFRList.Free;
        LSortedIFRList.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRDataValidator.GetMonthlyIFRFactor(AIFRFeature : TIFRFeature;AFactor,AMonthValue : Double; AMonth : integer) : double;
const OPNAME = 'TDailyIFRDataValidator.GetMonthlyIFRFactor';
var
  LIndex : integer;
  LVal1 : Double;
  LVal2 : Double;
  LVal3 : Double;
  LVal4 : Double;
  LFactor : Double;
  LValue : Double;
begin
  Result := 0;
  try
    if AIFRFeature <> nil then
    begin
      LFactor := 0;
      for LIndex :=  MinReleaseStructure to MaxReleaseStructure do
      begin
        LVal1 := AIFRFeature.InflowByIndexAndMonth[LIndex,AMonth]*AFactor;
        LVal2 := AIFRFeature.InflowByIndexAndMonth[LIndex+1,AMonth]*AFactor;
        LVal3 := AIFRFeature.ReleaseByIndexAndMonth[LIndex,AMonth]*AFactor;
        LVal4 := AIFRFeature.ReleaseByIndexAndMonth[LIndex+1,AMonth]*AFactor;
        if ((LVal2 - AMonthValue) > 0) then
        begin
          if ((LVal2) - (LVal1))<>0 then
            LFactor := (AMonthValue - (LVal1))/((LVal2) - (LVal1));
          LValue  := LFactor*((LVal4) - (LVal3))+(LVal3);
          if (AMonthValue<>0) then
            Result := LValue/AMonthValue;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRDataValidator.GetDailyIFRDataDialog : TDailyIFRDataDialog;
const OPNAME = 'TDailyIFRDataValidator.GetDailyIFRDataDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TDailyIFRDataDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyIFRDataValidator.SaveState: boolean;
const OPNAME = 'TDailyIFRDataValidator.SaveState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRDataValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,
                                                      ANewValue: string): boolean;
const OPNAME = 'TDailyIFRDataValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'InstreamAvgFlow') then
      DailyIFRDataDialog.btnGenerateDailyIFR.Enabled := (TDailyDiversionDataObject(FAppModules.Model.ModelData).
                                                         DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier].
                                                         DailyInstreamDataCount = 0);
    DailyIFRDataDialog.edtScaleFactor.ReadOnly := not DailyIFRDataDialog.btnGenerateDailyIFR.Enabled;
    DailyIFRDataDialog.edtScaleFactor.Color    := clSilver;
    if not DailyIFRDataDialog.btnGenerateDailyIFR.Enabled then
    begin
      FAppModules.ViewIni.WriteString(ClassName,'Station Name', TDailyDiversionDataObject(FAppModules.Model.ModelData).
                                                                DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier].
                                                                StationNo);
      FAppModules.ViewIni.WriteString(ClassName,'Channel Number',DailyIFRDataDialog.cbxChannel.Text);
      FAppModules.ViewIni.WriteString(ClassName,'CatchmentScaleFactor',DailyIFRDataDialog.edtScaleFactor.Text);
    end;
                                                         
    if (AFieldName = 'InstreamScaleFactor') then
    begin
      PopulateDataViewer;
      DoOncbxChannelSelect(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyIFRDataValidator.StudyHasChanged: boolean;
const OPNAME = 'TDailyIFRDataValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDailyIFRDataValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TDailyIFRDataValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyIFRDataValidator.PopulateDailyIFRForAllMonths;
const OPNAME = 'TDailyIFRDataValidator.PopulateDailyIFRForAllMonths';
var
  LDailyIFRData : TDailyIFRData;
  LIndex : integer;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
begin
  try
    if ExecGenerateDailyIFR(nil) then
    begin
      LDailyIFRData := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRData[
                       TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier].StationNo];
      if LDailyIFRData.DailyIFRList.Count > 0 then
      begin
        DailyIFRDataDialog.DailyIFRGrid.RowCount := LDailyIFRData.DailyIFRList.Count + 1;
        for LIndex := 0 to LDailyIFRData.DailyIFRList.Count-1 do
        begin
          LDailyInstreamFlowData := TDailyInstreamFlowData(LDailyIFRData.DailyIFRList.Objects[LIndex]);
          if LDailyInstreamFlowData <> nil then
          begin
            DailyIFRDataDialog.DailyIFRGrid.Cells[0,LIndex+1] := DateToStr(LDailyInstreamFlowData.InstreamDate);
            if LDailyInstreamFlowData.AvgFlow <> NullFloat then
              DailyIFRDataDialog.DailyIFRGrid.Cells[1,LIndex+1] := FloatToStr(LDailyInstreamFlowData.AvgFlow);
            DailyIFRDataDialog.DailyIFRGrid.RowHeights[LIndex+1] := 15;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataValidator.GenerateDailyIFR(Sender : TObject);
const OPNAME = 'TDailyIFRDataValidator.GenerateDailyIFR';
begin
  try
    FProgressDialog.ProgressRichEdit.Clear;
    FProgressDialog.clbOptions.Items.Clear;
    FProgressDialog.clbOptions.Items.Add(FAppModules.Language.GetString('CheckListBoxText.StopOnFirstError'));
//    FProgressDialog.clbOptions.Items.Add('Save to Database');
    FProgressDialog.clbOptions.Checked[0] :=  False;//FAppModules.GlobalData.StopOnFirstErr;
//    FProgressDialog.clbOptions.Checked[1] := True;
    FProgressDialog.Caption := FAppModules.Language.GetString('TDailyIFRDataValidator.strGenerateIFR');
    FProgressDialog.AddExecutionFunctions(ExecGenerateDailyIFR);
    FProgressDialog.FStartButton.Visible := True;;
    FProgressDialog.FStopButton.Visible := False;
    FProgressDialog.ActionProgressBar.Visible := True;
    FProgressDialog.ShowModal;
    FProgressDialog.Hide;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyIFRDataValidator.ExecGenerateDailyIFR(AProgressUpdateFuntion: TProgressUpdateFuntion): boolean;
const OPNAME = 'TDailyIFRDataValidator.ExecGenerateDailyIFR';
var
  LDiversionGauge : TDiversionGauge;
  LDailyFlowDataList : TStringList;
  LCalculatedIFRList : TStringList;
  LSortedIFRList : TStringList;
  LDailyIFRData : TDailyIFRData;
  LDailyFlowData : TDailyFlowData;
  LCalcFactor : double;
  LIFRFeature : TIFRFeature;
  LIndex : integer;
  LMonthlyFlowData : TMonthlyFlowData;
  LMonthIndex : integer;
  LPreYear : integer;
  LMonthValue : double;
  LMonth : word;
  LDay : word;
  LYear : word;
  LStop : boolean;
  LFactor : double;
  LDailyAvgFlow : double;
  LChannelNumber : integer;
  LDailyInstreamFlowData : TDailyInstreamFlowData;
  LHydroMonth : integer;
begin
  Result := False;
  try
    FProgressDialog.ActionProgressBar.Min := 0;
    FProgressDialog.FStopButton.Visible := True;
    LDailyIFRData := TDailyDiversionDataObject(FAppModules.Model.ModelData).DailyIFRData[
                     TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier].StationNo];
    LDiversionGauge := TDailyDiversionDataObject(FAppModules.Model.ModelData).
                       DailyDiversionGaugeDataList.DiversionGaugeByStationID[Identifier];
    LChannelNumber := 0;
    if DailyIFRDataDialog.cbxChannel.ItemIndex <> -1 then
      LChannelNumber := Integer(DailyIFRDataDialog.cbxChannel.Items.Objects[DailyIFRDataDialog.cbxChannel.ItemIndex]);
    LIFRFeature := LDailyIFRData.IFRFeatureList.CastMonthlyIFRFeatureByNumber(LChannelNumber);
    LStop := False;
    if (LDiversionGauge <> nil) and (LIFRFeature <> nil)then
    begin
      FProgressDialog.ActionProgressBar.Max := LDiversionGauge.DailyDataCount;
      LDailyFlowDataList := TStringList.Create;
      LCalculatedIFRList := TStringList.Create;
      LCalculatedIFRList.Sorted := True;
      LSortedIFRList := TStringList.Create;
      try
        for LMonthIndex := 1 to 12 do
        begin
          LDailyFlowDataList.Clear;
          LMonth := CHydroMonths[LMonthIndex];
          LDiversionGauge.GetDailyDiversionDataByMonth(LMonth,LDailyFlowDataList);
          if (LDailyFlowDataList.Count > 0) then
          begin
            LPreYear := 0;
            LMonthValue := 0;
            LHydroMonth := 0;
            LFactor := StrToFloat(DailyIFRDataDialog.edtScaleFactor.Text);
            for LIndex := 0 to LDailyFlowDataList.Count-1 do
            begin
              LDailyFlowData := TDailyFlowData(LDailyFlowDataList.Objects[LIndex]);
              if LDailyFlowData <> nil then
              begin
                DecodeDate(LDailyFlowData.DiversionDate,LYear,LMonth,LDay);
                FProgressDialog.UpdateProgressBar;
                if (FProgressDialog.Stopped) or (FAppModules.GlobalData.StopOnFirstErr)then
                  Exit;
                if Assigned(AProgressUpdateFuntion) then
                  AProgressUpdateFuntion(Format(FAppModules.Language.GetString('TDailyIFRDataValidator.ProcessingData'),[formatDateTime('yyyy/mm/dd',LDailyFlowData.DiversionDate)]),ptNone,LStop);
                if LStop then
                  Exit;
                LDailyAvgFlow := NullFloat;
                if LMonth < 10 then
                  LYear := LYear-1;

                if LYear <>  LPreYear then
                begin
                  LMonthlyFlowData := LDiversionGauge.MonthlyFlowDataByYear[LYear];
                  LPreYear := LYear;
                  if LMonthlyFlowData <> nil then
                  begin
                    if LMonth > 9 then
                      LHydroMonth := LMonth - 9
                    else
                      LHydroMonth := LMonth + 3;
                    LMonthValue := LMonthlyFlowData.AvgFlowByIndex[LMonthIndex];
                  end;
                end;
                LCalcFactor := GetMonthlyIFRFactor(LIFRFeature,LFactor,LMonthValue,LHydroMonth);
                if (LCalcFactor > 0) and (LDailyFlowData.FactoredFlow <> NullFloat) then
                  LDailyAvgFlow := StrToFloat(FormatFloat('##0.000',LDailyFlowData.FactoredFlow*LCalcFactor));
                LDailyInstreamFlowData := LDiversionGauge.AddInstreamFlowData(LDailyFlowData.DiversionDate);
                if LDailyInstreamFlowData <> nil then
                begin
                  LDailyInstreamFlowData.Poulate(LDailyFlowData.StationID,LDailyFlowData.Identifier,
                                        LDailyAvgFlow,LDiversionGauge.InstreamScaleFactor,
                                        LDiversionGauge.CompensationValueByIndex[LMonth],
                                        LDiversionGauge.CapacityOfDiversion,LDailyFlowData.QualityCode);
                  LCalculatedIFRList.AddObject(FloatToStr(LDailyFlowData.DiversionDate),LDailyInstreamFlowData);
                end;
              end;
            end;
          end;
        end;
        for LIndex := 0 to LCalculatedIFRList.Count-1 do
        begin
          LDailyInstreamFlowData := TDailyInstreamFlowData(LCalculatedIFRList.Objects[LIndex]);
          LSortedIFRList.AddObject(formatDateTime('yyyy/mm/dd',LDailyInstreamFlowData.InstreamDate),LDailyInstreamFlowData);
        end;
        LDiversionGauge.ClearDailyInstreamFlowFileData;
        LDailyIFRData.DailyIFRList.Assign(LSortedIFRList);
        if (Assigned(AProgressUpdateFuntion)) then
        begin
          if (LDailyIFRData.DailyIFRList.Count > 0) {and (FProgressDialog.clbOptions.Checked[1])} then
          begin
            if Assigned(AProgressUpdateFuntion) then
              AProgressUpdateFuntion(FAppModules.Language.GetString('TDailyIFRDataValidator.SavingDataToDatabase'),ptNone,LStop);
            TDailyDiversionDataObject(FAppModules.Model.ModelData).GenerateDailyIFR(LDiversionGauge,LDailyIFRData);
            if Assigned(AProgressUpdateFuntion) then
              AProgressUpdateFuntion(FAppModules.Language.GetString('TDailyIFRDataValidator.DoneSavingDataToDatabase'),ptNone,LStop);
          end;
        end;
      finally
        LDailyFlowDataList.Free;
        LCalculatedIFRList.Free;
        LSortedIFRList.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyIFRDataValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TDailyIFRDataValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
