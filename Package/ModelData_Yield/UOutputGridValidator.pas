//
//
//  UNIT      : Contains the class TOutputGridValidator.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UOutputGridValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Dialogs,
  Types,
  VCL.Grids,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UOutputGridDialog;

type
  TOutputGridValidator = class(TAbstractOutputDialogValidator)
  protected
    FCurrentViewData     : TOutputDataType;
    FPrevViewData        : TOutputDataType;
    FLoadCase            : integer;
    FSequence            : integer;
    FMonth               : integer;
    FUnits               : TOutputUnits;
    FValueType           : TOutputValueType;
    FDefValueType        : TOutputValueType;
    FTimeStep            : TOutputTimeStep;
    FHighLight           : WordBool;
    FDisplayMonth        : integer;
    FAtherChannelType    : boolean;
    procedure CreateMemberObjects; override;

    procedure OnViewDataTypeChange(Sender: TObject);
    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure PopulateDialogSelectors;
    procedure RePopulateDataViewer;
    procedure PopulateChannelGridData(AData: TStrings; AChannel : IGeneralFlowChannel);
    procedure PopulateChannelGrid(AData: TStrings);
    procedure PopulateChannelAreaGrid(AData: TStrings);
    procedure PopulateReservoirGrid(AData: TStrings);
    procedure PopulateReservoirGridData(AData: TStrings; AReservoir : IReservoirData);
    procedure PopulateReservoirAreaGrid(AData: TStrings);
    procedure RePopulateDDTSOutput;
    procedure GetSelectionData;
    function GetChannelNumberByChannelArea(AChannelAreaID : integer) : integer;
    function IsLastDayOfTheMonth (AYear,AMonth,ADay : integer):boolean;
    procedure ClearGrid;
    procedure BuildDDTSGrid;
    procedure SetDDTSOutputDataViewers;
    procedure SetCurrentViewData;
    procedure ChangeViewDataLabel;
    procedure DoDrawGrid (Sender     : TObject;
                          ACol, ARow : Longint;
                          Rect       : TRect;
                          State      : TGridDrawState);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    function GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    function GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer; override;
    function ViewDialog: TOutputGridDialog;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    property CurrentViewData: TOutputDataType  read FCurrentViewData;

  end;

implementation

uses
  VCLTee.Series,
  Windows,
  SysUtils,
  UOutputData,
  UDataSetType,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, Variants, UNetworkElementData,
  UChannelData,
  UDDTSData,
  UDDTSDataObject,
  UReservoirData;

{ TOutputGridValidator }

procedure TOutputGridValidator.CreateMemberObjects;
const OPNAME = 'TOutputGridValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputGridDialog.Create(FPanelOwner,FAppModules);
    ViewDialog.cmbViewDataType.OnSelect:= OnViewDataTypeChange;
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    ViewDialog.grdDataGrid.OnDrawCell   := DoDrawGrid;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.Initialise: boolean;
const OPNAME = 'TOutputGridValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    FPrevViewData    := btNone;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.IsLastDayOfTheMonth(AYear, AMonth,
  ADay: integer): boolean;
const OPNAME = 'TOutputGridValidator.StudyDataHasChanged';
var
  LLastDayOfTheMonth : integer;
begin
  Result := False;
  try
    LLastDayOfTheMonth := MonthDays[IsLeapYear(AYear),AMonth];
    Result := (ADay=LLastDayOfTheMonth);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputGridValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'LOADCASESCOUNT') or
      (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  or
      (UpperCase(AFieldName) = 'OUTPUTDATASELECTION')then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputGridValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputGridValidator.LanguageHasChanged';
begin
  Result := False;
  try
    if(NetworkElementType in [votMasterControl,votChannel]) then
      TabShetCaption := 'Supply'
    else
      TabShetCaption := 'Grid';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.SaveState: boolean;
const OPNAME = 'TOutputGridValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.ViewDialog : TOutputGridDialog;
const OPNAME = 'TOutputGridValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputGridDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputGridValidator.GetNextSignificantRecord';
begin
  Result := 0;
  try
    Result := ACurrentRecord  + 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputGridValidator.GetPreviousSignificantRecord';
begin
  Result := 0;
  try
    Result := ACurrentRecord  - 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.ClearDataViewer;
const OPNAME = 'TOutputGridValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearGrid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.PopulateDataViewer;
const OPNAME = 'TOutputGridValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
//    FPrevViewData           := FCurrentViewData;
//    FPrevNetworkElementType := FNetworkElementType;
    ClearDataViewer;
//    ViewDialog.cmbViewDataType.Items.Clear;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputGridValidator.PopulateDialogSelectors';
var
  LViewData    : string;
  lIndex       : integer;
  lSelectIndex : integer;
begin
  try
    ViewDialog.cmbViewDataType.Items.Clear;
    if(FAppModules.Model.ModelName = CDDTS) then
    begin
      LViewData := 'Storage volume';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(0)));
      LViewData := 'Inflows';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(1)));
      LViewData := 'Rainfall on dam surface';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(2)));
      LViewData := 'Evaporation from dam surface';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(3)));
      LViewData := 'Dam spills';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(4)));
      LViewData := 'EWR Supply From Dam Release';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(5)));
      LViewData := 'EWR Supply From Dam Spills';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(6)));
      LViewData := 'EWR Supply From D/s Runoff';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(7)));
      LViewData := 'D/s Target';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(8)));
      LViewData := 'D/s Supply';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(9)));
      LViewData := 'Abstruction Target';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(10)));
      LViewData := 'Abstruction Supply';
      ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(11)));
      ViewDialog.BtnDataSelection.Visible := False;
      ViewDialog.cmbViewDataType.ItemIndex := 0;
      Exit;
    end;

    if (FIdentifier >= 0)  and (NetworkElementType <> votNone) then
    begin
      case NetworkElementType of
        votMasterControl:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile11');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));

        end;
        votReservoir,votReservoirAreaGroup :
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile1');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirVolume)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile2');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirElevation)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile3');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btNetBasinRunoffIntoResArea)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile4');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btRainfallOnReservoirSurface)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile5');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btGrossEvaporationLossFromReservoir)));
        end;
        votNodeWithInflow:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile3');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btNetBasinRunoffIntoResArea)));
        end;
        votNodeWithoutInflow:
        begin
        end;
        votChannel,votChannelArea:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile11');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageChannelFlow)));
        end;
        votIrrigationArea:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile9');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageIrrigationDeficits)));
        end;
        votPowerPlant:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile8');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthlyAverageStackedEnergy)));
        end;
        votWetland:
        begin
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile1');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirVolume)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile2');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btMonthEndReservoirElevation)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile3');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btNetBasinRunoffIntoResArea)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile4');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btRainfallOnReservoirSurface)));
          LViewData := FAppModules.Language.GetString('ViewData.SumOutYearFile5');
          ViewDialog.cmbViewDataType.Items.AddObject(LViewData,TObject(Ord(btGrossEvaporationLossFromReservoir)));
        end;
      end;
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

procedure TOutputGridValidator.RePopulateDataViewer;
const OPNAME = 'TOutputGridValidator.RePopulateDataViewer';
var
  LErrors        : string;
  LDataContainer : TStringList;
  LIrrigationArea : IIrrigationArea;
begin
  try
    ClearGrid;
    if(FAppModules.Model.ModelName = CDDTS) then
    begin
      SetDDTSOutputDataViewers;
      RePopulateDDTSOutput;
      Exit;
    end;
    GetSelectionData;
    ChangeViewDataLabel;
    if (FIdentifier        >= 0) and
       (NetworkElementType <> votNone) and
       (FCurrentViewData   <> btNone) then
    begin
      LDataContainer := TStringList.Create;
      try
        LErrors := '';
        ViewDialog.ShowError(LErrors);

        if (NetworkElementType = votChannelArea) then
        begin
          if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetChannelAreaData(
            LDataContainer,FCurrentViewData,FIdentifier,LErrors)) then
              PopulateChannelAreaGrid(LDataContainer);
        end
        else
        if (NetworkElementType = votReservoirAreaGroup) then
        begin
          if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetReservoirAreaData(
            LDataContainer,FCurrentViewData,FIdentifier,LErrors)) then
              PopulateReservoirAreaGrid(LDataContainer);
        end

        else
        if (NetworkElementType = votIrrigationArea) then
        begin
          LIrrigationArea := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.IrrigationAreaList.
                             IrrigationAreaByNodeNumber[FIdentifier];
          if(LIrrigationArea <> nil) then
          begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
              LDataContainer,FCurrentViewData,LIrrigationArea.FeatureID,LErrors)) then
                PopulateReservoirGrid(LDataContainer);
          end;
        end
        else
        if (NetworkElementType = votPowerPlant) then
        begin
            if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
              LDataContainer,FCurrentViewData,FIdentifier,LErrors)) then
                PopulateReservoirGrid(LDataContainer);
        end
        else
        begin
          if(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.GetBlockData(
            LDataContainer,FCurrentViewData,FIdentifier,LErrors)) then
          begin
            if (NetworkElementType in [votMasterControl, votChannel]) then
              PopulateChannelGrid(LDataContainer)
            else
            if (NetworkElementType = votReservoirAreaGroup) then
              PopulateReservoirAreaGrid(LDataContainer)
            else
              PopulateReservoirGrid(LDataContainer)
          end;
        end;
      finally
        LDataContainer.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.RePopulateDDTSOutput;
const OPNAME = 'TOutputGridValidator.RePopulateDDTSOutput';
var
  LSOutputDataList : TDDTSOutputDataList;
  LSOutputData : TDDTSOutputData;
  LIndex : integer;
  LMonth : word;
  LDay : word;
  LYear : word;
  LRow,
  LCol : integer;
  LValue : double;
begin
  try
    LockWindowUpdate(FPanel.Handle);
    try
      ClearGrid;
      BuildDDTSGrid;
      LSOutputDataList :=  TDDTSDataObject(FAppModules.Model.ModelData).DDTSOutputDataList;
      if LSOutputDataList <> nil then
      begin
        ViewDialog.grdDataGrid.Cells[0,0] := 'Year/Month';
        for LIndex := 1 to 32 do
          ViewDialog.grdDataGrid.Cells[LIndex,0] := IntTostr(LIndex);
        LCol := 1;
        LRow := ViewDialog.grdDataGrid.RowCount-1;
        for LIndex := 0 to LSOutputDataList.Count-1 do
        begin
          LSOutputData := LSOutputDataList.OutputDataByIndex[LIndex];
          decodedate(LSOutputData.ColumnA, LYear, LMonth, LDay);
          LValue := 0;
          case ViewDialog.cmbViewDataType.ItemIndex of
            // Storage volume
            0: LValue := LSOutputData.ColumnAJ;
            //   Inflows
            1: LValue := LSOutputData.ColumnI;
            //  Rainfall on dam surface
            2: LValue := LSOutputData.ColumnH;
            //  Evaporation from dam surface
            3: LValue := LSOutputData.ColumnM;
            // Dam spills
            4: LValue := LSOutputData.ColumnAL;
            // EWR Supply From Dam Release
            5: LValue := LSOutputData.ColumnX;
            // EWR Supply From Dam Spills
            6: LValue := LSOutputData.ColumnV;
            // EWR Supply From D/s Runoff
            7: LValue := LSOutputData.ColumnU;
            // D/s Target
            8: LValue := LSOutputData.ColumnAC;
            // D/s Supply
            9: LValue := LSOutputData.ColumnAD;
            //  Abstruction Target
            10: LValue := LSOutputData.ColumnAF;
            //  Abstruction Supply
            11: LValue := LSOutputData.ColumnAG;

          end;
          ViewDialog.grdDataGrid.Cells[LCol,LRow] := FormatFloat('###00.000',LValue);
          ViewDialog.grdDataGrid.Cells[0,LRow] :=  FormatDateTime('yyyy/mm',LSOutputData.ColumnA);
          LCol := LCol+1;
          if IsLastDayOfTheMonth(LYear,LMonth,LDay) then
          begin
            LCol := 1;
            ViewDialog.grdDataGrid.RowCount := ViewDialog.grdDataGrid.RowCount + 1;
            LRow := ViewDialog.grdDataGrid.RowCount-1;
          end;
        end;
      end;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.SetCurrentViewData;
const OPNAME = 'TOutputGridValidator.SetCurrentViewData';
var
  LIndex : integer;
begin
  try
{    LIndex := Integer(ViewDialog.cmbViewDataType.Items.Objects[ViewDialog.cmbViewDataType.ItemIndex]);
    if (LIndex < 0) then
      LIndex := 0;}
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

procedure TOutputGridValidator.SetDDTSOutputDataViewers;
const OPNAME = 'TOutputGridValidator.ChangeViewDataLabel';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.ChangeViewDataLabel;
const OPNAME = 'TOutputGridValidator.ChangeViewDataLabel';
begin
  try
    if (FCurrentViewData = btMonthlyAverageChannelFlow)  then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')'
      else if (FUnits = ouMcmPerMonthOrYear) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perMonth')+')'
      else if (FUnits = ouPercentage) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('ChangeLists.Percentage')+')'
      else if (FUnits = ouMegaLitersPerDay) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.MegaL')+')'
      else
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
    end;

    if (FCurrentViewData = btMonthEndReservoirElevation) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('TField.m')
      else if (FUnits = ouPercentage) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('ChangeLists.Percentage')
      else
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('TField.m');
    end;

    if (FCurrentViewData = btMonthEndReservoirVolume) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := LowerCase(FAppModules.Language.GetString('TField.MCM'))
      else if (FUnits = ouPercentage) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('ChangeLists.Percentage')
      else
        ViewDialog.UnitsLabel.Caption := LowerCase(FAppModules.Language.GetString('TField.MCM'))
     end;

    if (FCurrentViewData = btNetBasinRunoffIntoResArea) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')'
      else if (FUnits = ouMcmPerMonthOrYear) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.M3perMonth')
      else if (FUnits = ouPercentage) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('ChangeLists.Percentage')
      else if (FUnits = ouMegaLitersPerDay) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.MegaL')
      else
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
    end;

    if (FCurrentViewData = btRainfallOnReservoirSurface) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')'
      else if (FUnits = ouMcmPerMonthOrYear) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.M3perMonth')
      else if (FUnits = ouPercentage) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('ChangeLists.Percentage')
      else if (FUnits = ouMegaLitersPerDay) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.MegaL')
      else
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
    end;

    if (FCurrentViewData = btGrossEvaporationLossFromReservoir) then
    begin
      if (FUnits = ouPerSecond) then
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')'
      else if (FUnits = ouMcmPerMonthOrYear) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.M3perMonth')
      else if (FUnits = ouPercentage) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('ChangeLists.Percentage')
      else if (FUnits = ouMegaLitersPerDay) then
        ViewDialog.UnitsLabel.Caption := FAppModules.Language.GetString('MasterControl.MegaL')
      else
        ViewDialog.UnitsLabel.Caption := '('+FAppModules.Language.GetString('MasterControl.M3perSecond')+')';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputGridValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputGridValidator.OnViewDataTypeChange';
begin
  try
    SetCurrentViewData;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.PopulateChannelGrid(AData: TStrings);
const OPNAME = 'TOutputGridValidator.PopulateChannelGrid';
var
  lChannel : IGeneralFlowChannel;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    if (lChannel <> nil) then
    begin
      PopulateChannelGridData(AData,lChannel);
      if (FUnits = ouMcmPerMonthOrYear) then
         CalculateMCMPerMonthGridValues(ViewDialog.grdDataGrid);
      if (FUnits = ouPercentage) then
      begin
//        ShowMessage('Percentage is not allowed because there is no demand for selected channels')
      end
      else
      if(FValueType = ovtDeficits) then
      begin
//        ShowMessage(('Deficit is not allowed because there is no demand for selected channels'));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.PopulateChannelAreaGrid(AData: TStrings);
const OPNAME = 'TOutputGridValidator.PopulateChannelAreaGrid';
var
  lChannel : IGeneralFlowChannel;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                CastNetworkElementData.CastChannelList.CastChannelNumberFromChannelID[FIdentifier];
    if (lChannel <> nil) then
    begin
      PopulateChannelGridData(AData,lChannel);
      if (FUnits = ouMcmPerMonthOrYear) then
         CalculateMCMPerMonthGridValues(ViewDialog.grdDataGrid);
      if (FUnits = ouPercentage) then
      begin
//        ShowMessage('Percentage is not allowed because there is no demand for selected channels');
      end
      else
      if( FValueType = ovtDeficits) then
      begin
//        ShowMessage(('Deficit is not allowed because there is no demand for selected channels'));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TOutputGridValidator.PopulateChannelGridData(AData: TStrings;AChannel : IGeneralFlowChannel);
const OPNAME = 'TOutputGridValidator.PopulateChannelGridData';
var
  LIndex,
  LCount          : integer;
  LMonthlyValues  : TStringList;
  LSupply         : double;
  LAvarages       : array[1..12] of double;
  LNeedDemand     : boolean;
  LDemand         : double;
  LFactor         : double;
  lMonthDays      : array [1..12] of double;
  lDaysInAYear    : double;
  lYieldModelData : IYieldModelData;
  LMonthValue     : double;
  lDemandFeature  : ISpecifiedDemandFeature;
  lDemandStr      : WideString;
  lTimeStep       : integer;
  LTotal          : double;
  LGrandAvarage   : double;
  //LFieldProperty  : TAbstractFieldProperty;
  LHasDeficit     : Boolean;
  LStartMonth     : integer;
begin
  try
    LockWindowUpdate(FPanel.Handle);
    try
      ClearGrid;
      FValueType := ovtSupply;
      lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
      ViewDialog.grdDataGrid.RowCount := Max(2,AData.Count + 2);
      FAtherChannelType := False;
      if (AData.Count > 0) then
      begin
        ViewDialog.grdDataGrid.Cells[0,0] := FAppModules.Language.GetString('MonthGrid.Year');
        ViewDialog.grdDataGrid.Cells[13,0] := FAppModules.Language.GetString('MonthGrid.Average');

        LStartMonth := LYieldModelData.RunConfigurationData.StartMonthNumber - 1; // + FAppModules.StudyArea.CalendarStartMonth + 1;

        for LIndex := 1 to 12 do
        begin
          LStartMonth := LStartMonth + 1;
          if(LStartMonth > 12) then
            LStartMonth := 1;
          ViewDialog.grdDataGrid.Cells[LIndex,0] :=
          lYieldModelData.RunConfigurationData.MonthNameByIndex[LStartMonth];
        end;

        for LIndex := Low(LAvarages) to High(LAvarages) do
          LAvarages[LIndex] := 0.0;
        for LIndex := 1 to 12 do
          lMonthDays[lIndex] := lYieldModelData.RunConfigurationData.MonthDaysByIndex[lIndex];
        lDaysInAYear := TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;

        LNeedDemand := (FUnits = ouPercentage) or (FValueType = ovtDeficits);
        LDemand     := 0.0;
        lTimeStep   := 0;
        LHasDeficit := False;
        FAtherChannelType := (AChannel.ChannelType <> 2) and (AChannel.ChannelType <> 8) and
                             (AChannel.ChannelType <> 11) and (AChannel.ChannelType <> 14);

        //LFieldProperty :=  FAppModules.FieldProperties.FieldProperty('FlowConstraints');
        LMonthlyValues := TStringList.Create;
        try
          for LIndex := 0 to AData.Count -1 do
          begin
            LTotal := 0.0;
            LMonthlyValues.CommaText := AData.Strings[LIndex];
            ViewDialog.grdDataGrid.Cells[0,LIndex+1] := LMonthlyValues.Strings[0];
            for LCount := 1 to 12 {LMonthlyValues.Count -1} do
            begin
              lTimeStep  := lTimeStep + 1;
              LSupply := StrToFloat(LMonthlyValues.Strings[LCount]);

              if LNeedDemand then
              begin
                if (AChannel.ChannelType = 2) then  //yield
                begin
                  LDemand := lYieldModelData.RunConfigurationData.TargetYieldByIndex[FLoadCase];
                  LFactor := lYieldModelData.NetworkFeaturesData.MasterControlFeatureList.MasterControlFeatureByIndex[0].FactorByMonth[LCount];
                  LDemand := (LDemand * LFactor * 1000000.0)/(365.25 * 86400.0);  {m3/s}
                  LDemand := StrToFloat(FormatFloat('0.000',LDemand));
                end

                else if (AChannel.ChannelType = 8) then  // minmax
                begin
                  LDemand := AChannel.MinMaxFlowConstraint.FlowConstraintByArcMonth[1, LCount];
                end

                else if (AChannel.ChannelType = 11) then // specified Demand
                begin
                  lDemandFeature := AChannel.SpecifiedDemandFeature;
                  if (lDemandFeature.GetMonthlyDemand(lTimeStep,lDemandStr)) then
                    LDemand := Round((StrToFloat(lDemandStr) * 1000000 / (lMonthDays[LCount] * 86400.0)) * 1000) / 1000
                  else
                    LDemand := LSupply;
                end
                else
                if FAtherChannelType then
                begin
                  if (FValueType = ovtDeficits) then
                  begin
                    FDefValueType := FValueType;
                    FValueType := ovtSupply;
                  end;
                end;
              end;

              if (FUnits = ouPercentage) then
              begin
                if (FValueType = ovtDeficits) then {ovtDeficit}
                begin
                  if LDemand > 0 then
                    LMonthValue := (LDemand - LSupply)/LDemand * 100.0
                  else
                    LMonthValue := 0;
                end
                else {ovtSupply}
                begin
                  if (LDemand > 0) then
                    LMonthValue := LSupply/LDemand * 100
                  else
                    LMonthValue := 0;
                end;
              end
              else
              if (FUnits = ouPerSecond) then
              begin
                if (FValueType = ovtDeficits) then {ovtDeficit}
                  LMonthValue := LDemand - LSupply
                else {ovtSupply}
                  LMonthValue := LSupply;
              end
              else
              if (FUnits = ouMcmPerMonthOrYear) then
              begin
                if (FValueType = ovtDeficits) then {ovtDeficit}
                  LMonthValue  := (LDemand - LSupply) * (lMonthDays[LCount] * 86400.0) /1000000.0
                else {ovtSupply}
                  LMonthValue  := LSupply * (lMonthDays[LCount] * 86400.0) /1000000.0;
              end
              else
              if (FUnits = ouMegaLitersPerDay) then
              begin
                if (FValueType = ovtDeficits) then {ovtDeficit}
                  LMonthValue  := (LDemand - LSupply) *  86400.0 / 1000.0
                else {ovtSupply}
                  LMonthValue  := (LSupply  * 86400.0) / 1000.0;
              end
              else
              begin
                if (FValueType = ovtSupply) then
                  LMonthValue := LSupply
                else {ovtDeficit}
                  LMonthValue := LDemand - LSupply;
              end;

              if LNeedDemand then
              begin
                if (AChannel.ChannelType = 2) or (AChannel.ChannelType = 8) or (AChannel.ChannelType = 11) then
                begin
                  if LMonthValue <> 0 then
                    LHasDeficit := True;
                end;
              end;
              LTotal :=  LTotal + (LMonthValue * lMonthDays[LCount]);
              LAvarages[LCount] := LAvarages[LCount] + LMonthValue;
              ViewDialog.grdDataGrid.Cells[LCount,LIndex+1] := FormatOutputValue(FCurrentViewData,LMonthValue);
            end;
            LTotal := LTotal / lDaysInAYear;
            ViewDialog.grdDataGrid.Cells[13,LIndex+1] := FormatOutputValue(FCurrentViewData,LTotal);;
          end;
          if not LHasDeficit then
          begin
            if (FValueType = ovtDeficits) then
            begin
              if (AChannel.ChannelType = 2) or (AChannel.ChannelType = 8) or (AChannel.ChannelType = 11) then
                ShowMessage( FAppModules.Language.GetString('Message.NoDeficitForSelectedChannel'));
            end;
          end;
          LGrandAvarage := 0.00;
          for LIndex := Low(LAvarages) to High(LAvarages) do
          begin
            LAvarages[LIndex] := LAvarages[LIndex] / AData.Count ;
            ViewDialog.grdDataGrid.Cells[LIndex,ViewDialog.grdDataGrid.RowCount-1] :=
                FormatOutputValue(FCurrentViewData,LAvarages[LIndex]);
            LGrandAvarage := LGrandAvarage + (LAvarages[LIndex]  * lMonthDays[LIndex]);
          end;
          LGrandAvarage := LGrandAvarage / lDaysInAYear;
          ViewDialog.grdDataGrid.Cells[0,ViewDialog.grdDataGrid.RowCount-1] :=
                                     FAppModules.Language.GetString('MonthGrid.Average');
          ViewDialog.grdDataGrid.Cells[13,ViewDialog.grdDataGrid.RowCount-1] :=
                                     FormatOutputValue(FCurrentViewData,LGrandAvarage);
        finally
          LMonthlyValues.Free;
        end;
      end;

      if FAtherChannelType then
      begin
        if (FDefValueType = ovtDeficits) then
          FValueType := FDefValueType;
      end;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.PopulateReservoirGrid(AData: TStrings);
const OPNAME = 'TOutputGridValidator.PopulateReservoirGrid';
var
  LIndex,
  LCount                : integer;
  LFullSupplyElevation  : double;
  LBottomLevel          : double;
  LDeadStorageLevel     : double;
  LReservoirVolume      : double;
  LTotal                : double;
  LMonthlyValue         : double;
  LMonthlyValues        : TStringList;
  LAvarages             : array[0..13] of double;
  LReservoirData        : IReservoirData;
  //LFieldProperty        : TAbstractFieldProperty;
  LGrandAvarage         : double;
begin
  try
    GetSelectionData;
    LockWindowUpdate(FPanel.Handle);
    try
      ClearGrid;
      ViewDialog.grdDataGrid.RowCount := Max(2,AData.Count + 2);
      LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                             ReservoirList.ReservoirByIdentifier[FIdentifier];
      if LReservoirData <> nil then
      begin

      end;

      if (AData.Count > 0) then
      begin
        ViewDialog.grdDataGrid.Cells[0,0] := 'Year';
        ViewDialog.grdDataGrid.Cells[13,0] := 'Average';
        for LIndex := 1 to 12 do
        begin
          ViewDialog.grdDataGrid.Cells[LIndex,0] :=
          TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LIndex];
        end;

        for LIndex := Low(LAvarages) to High(LAvarages) do
          LAvarages[LIndex] := 0.0;

        LMonthlyValues := TStringList.Create;
        try
          for LIndex := 0 to AData.Count -1 do
          begin
            LTotal := 0.0;
            LMonthlyValues.CommaText := AData.Strings[LIndex];
            ViewDialog.grdDataGrid.Cells[0,LIndex+1] := LMonthlyValues.Strings[0];

             for LCount := 1 to 12 do
            begin
              LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);

              if (FCurrentViewData = btNetBasinRunoffIntoResArea) or
                 (FCurrentViewData = btRainfallOnReservoirSurface) or
                 (FCurrentViewData = btGrossEvaporationLossFromReservoir) then
              begin
                if (FUnits = ouPerSecond) then
                  LMonthlyValue := LMonthlyValue;
                if (FUnits = ouMcmPerMonthOrYear) then
                  LMonthlyValue  := (LMonthlyValue *  86400.0) /1000000.0;
                if (FUnits = ouMegaLitersPerDay) then
                  LMonthlyValue  := (LMonthlyValue *  86400.0) /1000.0;
              end;

              if (FUnits = ouPercentage) then
              begin
                if (FCurrentViewData = btMonthEndReservoirElevation) then
                begin
                  LFullSupplyElevation :=  LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
                  LBottomLevel := LReservoirData.ReservoirZoneElevationsData.BottomOfReservoir.Elevation;
                  if (LFullSupplyElevation-LBottomLevel) > 0 then
                    LMonthlyValue := (LMonthlyValue-LBottomLevel)/(LFullSupplyElevation-LBottomLevel) * 100.0;

                end
                else if (FCurrentViewData = btMonthEndReservoirVolume) then
                begin
                  LReservoirVolume := LReservoirData.ReservoirConfigurationData.VolumeWhenFull;
                  if LReservoirVolume > 0 then
                    LMonthlyValue    := LMonthlyValue/LReservoirVolume * 100.0;
                end
                else
                  if LMonthlyValue > 0 then
                    LMonthlyValue := LMonthlyValue/LMonthlyValue * 100.0;
              end;

              if (FUnits = ouLivePercentage) then
              begin
                if (FCurrentViewData = btMonthEndReservoirElevation) then
                begin
                  LFullSupplyElevation := LReservoirData.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
                  LDeadStorageLevel := LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation;

                  if ((LFullSupplyElevation-LDeadStorageLevel) > 0) then
                    LMonthlyValue := (LMonthlyValue-LDeadStorageLevel)/(LFullSupplyElevation-LDeadStorageLevel) * 100.0
                  else
                    LMonthlyValue := 0.0;

                  if LMonthlyValue < 0 then
                    LMonthlyValue := 0.0;
                end
                else if (FCurrentViewData = btMonthEndReservoirVolume) then
                begin

                  LReservoirVolume := LReservoirData.ReservoirConfigurationData.VolumeWhenFull;
                  LDeadStorageLevel := LReservoirData.GetReservoirVolumeByElevation(LReservoirData.ReservoirZoneElevationsData.DeadStorageLevel.Elevation);
                  if (LReservoirVolume-LDeadStorageLevel) > 0 then
                    LMonthlyValue    := (LMonthlyValue-LDeadStorageLevel)/(LReservoirVolume-LDeadStorageLevel) * 100.0
                  else
                    LMonthlyValue    := 0.0;

                  if LMonthlyValue < 0 then
                    LMonthlyValue := 0.0;

                end
                else
                  if LMonthlyValue > 0 then
                    LMonthlyValue := LMonthlyValue/LMonthlyValue * 100.0;
              end;



              LTotal :=  LTotal + (LMonthlyValue * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LCount]);

              LAvarages[LCount] := LAvarages[LCount] + LMonthlyValue;
              //LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
              ViewDialog.grdDataGrid.Cells[LCount,LIndex+1] := FormatOutputValue(FCurrentViewData,LMonthlyValue);
            end;
            LTotal := LTotal / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;
            ViewDialog.grdDataGrid.Cells[13,LIndex+1] := FormatOutputValue(FCurrentViewData,LTotal);
          end;
          LGrandAvarage := 0.00;
          //LFieldProperty    := FAppModules.FieldProperties.FieldProperty('FlowConstraints');
          for LIndex := Low(LAvarages)+1 to High(LAvarages) do
          begin
            LAvarages[LIndex] := LAvarages[LIndex] / AData.Count;
            ViewDialog.grdDataGrid.Cells[LIndex,ViewDialog.grdDataGrid.RowCount-1] :=  FormatOutputValue(FCurrentViewData,LAvarages[LIndex]);
            LGrandAvarage := LGrandAvarage + (LAvarages[LIndex] * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LIndex]);;
          end;
           LGrandAvarage := LGrandAvarage / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;
           ViewDialog.grdDataGrid.Cells[13,ViewDialog.grdDataGrid.RowCount-1] := FormatOutputValue(FCurrentViewData,LGrandAvarage);
           ViewDialog.grdDataGrid.Cells[0,ViewDialog.grdDataGrid.RowCount-1] := FAppModules.Language.GetString('MonthGrid.Average');
          finally
          LMonthlyValues.Free;
        end
      end;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.GetSelectionData;
const OPNAME = 'TOutputGridValidator.GetSelectionData';
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
     FDefValueType := FValueType;
     FTimeStep     := LDataSelection.TimeStep;
     FHighLight    := LDataSelection.Highlight;
     FDisplayMonth := LDataSelection.DisplayMonth;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.ClearGrid;
const OPNAME = 'TOutputGridValidator.ClearGrid';
var
  LIndex: integer;
begin
  try
    for LIndex := 0 to ViewDialog.grdDataGrid.ColCount -1 do
      ViewDialog.grdDataGrid.Cols[LIndex].Clear;
    ViewDialog.grdDataGrid.ColCount := 14;
    ViewDialog.grdDataGrid.RowCount := 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.BuildDDTSGrid;
const OPNAME = 'TOutputGridValidator.BuildDDTSGrid';
var
  LIndex: integer;
begin
  try
    for LIndex := 0 to ViewDialog.grdDataGrid.ColCount -1 do
      ViewDialog.grdDataGrid.Cols[LIndex].Clear;
    ViewDialog.grdDataGrid.ColCount := 32;
    ViewDialog.grdDataGrid.RowCount := 2;



  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.CanExport: boolean;
const OPNAME = 'TOutputGridValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGridValidator.CanPrint: boolean;
const OPNAME = 'TOutputGridValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputGridValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.DoPrint;
const OPNAME = 'TOutputGridValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputGridValidator.OnBtnDataSelectionClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdGrid,FCurrentViewData,FValueType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.DoDrawGrid (Sender     : TObject;
                                           ACol, ARow : Longint;
                                           Rect       : TRect;
                                           State      : TGridDrawState);
const OPNAME = 'TOutputGridValidator.DoDrawGrid';
var
  lStrValue      : string;
  lDblValue      : double;
  lChannel       : IGeneralFlowChannel;
begin
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).
                NetworkElementData.ChannelList.ChannelByChannelNumber[FIdentifier];
    with ViewDialog do
    begin
      if (FHighLight) then
      begin
        if (lChannel <>  nil) then
        begin
          if (lChannel.ChannelType in [2,8,11]) then
          begin
            if (NetworkElementType in [votMasterControl, votChannel]) AND (ACol > 0) AND (ARow > 0) then
            begin
              lStrValue := Trim(grdDataGrid.Cells[ACol, ARow]);
              if (lStrValue <> '') then
              begin
                lDblValue := StrToFloat(lStrValue);
                if ((FValueType = ovtDeficits) AND (FUnits = ouPercentage)  AND (lDblValue > 0)) then
                  grdDataGrid.Canvas.Font.Color := clRed;
                if ((FValueType = ovtDeficits) AND (FUnits <> ouPercentage) AND (lDblValue > 0)) then
                  grdDataGrid.Canvas.Font.Color := clRed;
                if ((FValueType = ovtSupply)   AND (FUnits = ouPercentage) AND (lDblValue < 100)) then
                  grdDataGrid.Canvas.Font.Color := clRed
              end;
            end
            else
              grdDataGrid.Canvas.Font.Color := clBlack;
            grdDataGrid.Canvas.TextRect(Rect, Rect.Left, Rect.Top, grdDataGrid.Cells[ACol, ARow]);
          end;  
        end;
      end;
    end;                                       
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputGridValidator.GetChannelNumberByChannelArea(AChannelAreaID: integer): integer;
const OPNAME = 'TOutputGridValidator.GetChannelNumberByChannelArea';
var
  lChannel     : IGeneralFlowChannel;
begin
  result := 0;
  try
    lChannel := TYieldModelDataObject(FAppModules.Model.ModelData).CastNetworkElementData
                .CastChannelList.CastChannelNumberFromChannelID[FIdentifier];
    if (lChannel <> nil) then
      Result := lChannel.ChannelNumber;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputGridValidator.PopulateReservoirAreaGrid(AData: TStrings);
const OPNAME = 'TOutputGridValidator.PopulateReservoirAreaGrid';
var
  LReservoirData : TReservoirData;
begin
  try
    LReservoirData := TYieldModelDataObject(FAppModules.Model.ModelData).
                CastNetworkElementData.CastReservoirList.CastReservoirByReservoirGroupID[FIdentifier];
    if (LReservoirData <> nil) then
    begin
      PopulateReservoirGridData(AData,LReservoirData);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGridValidator.PopulateReservoirGridData(AData: TStrings;AReservoir: IReservoirData);
const OPNAME = 'TOutputGridValidator.PopulateReservoirGridData';
var
  LIndex,
  LCount                : integer;
  LFullSupplyElevation  : double;
  LReservoirVolume      : double;
  LTotal                : double;
  LMonthlyValue         : double;
  LMonthlyValues        : TStringList;
  LAvarages             : array[0..13] of double;
  LGrandAvarage         : double;
begin
  try
    GetSelectionData;
    LockWindowUpdate(FPanel.Handle);
    try
      ClearGrid;
      ViewDialog.grdDataGrid.RowCount := Max(2,AData.Count + 2);
      if (AData.Count > 0) then
      begin
        ViewDialog.grdDataGrid.Cells[0,0] := 'Year';
        ViewDialog.grdDataGrid.Cells[13,0] := 'Average';
        for LIndex := 1 to 12 do
        begin
          ViewDialog.grdDataGrid.Cells[LIndex,0] :=
          TYieldModelDataObject(FAppModules.Model.ModelData).RunConfigurationData.MonthNameByIndex[LIndex];
        end;

        for LIndex := Low(LAvarages) to High(LAvarages) do
          LAvarages[LIndex] := 0.0;

        LMonthlyValues := TStringList.Create;
        try
          for LIndex := 0 to AData.Count -1 do
          begin
            LTotal := 0.0;
            LMonthlyValues.CommaText := AData.Strings[LIndex];
            ViewDialog.grdDataGrid.Cells[0,LIndex+1] := LMonthlyValues.Strings[0];

            for LCount := 1 to 12 do
            begin
              LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);

              if (FUnits = ouPerSecond) then
                LMonthlyValue := LMonthlyValue;
              if (FUnits = ouMcmPerMonthOrYear) then
                LMonthlyValue  := (LMonthlyValue *  86400.0) /1000000.0;
              if (FUnits = ouMegaLitersPerDay) then
                LMonthlyValue  := (LMonthlyValue *  86400.0) /1000.0;

              if (FUnits = ouPercentage) then
              begin
                if (FCurrentViewData = btMonthEndReservoirElevation) then
                begin
                  LFullSupplyElevation :=  AReservoir.ReservoirZoneElevationsData.FullSupplyLevel.Elevation;
                  if LFullSupplyElevation > 0 then
                    LMonthlyValue := LMonthlyValue/LFullSupplyElevation * 100.0;
                end
                else if (FCurrentViewData = btMonthEndReservoirVolume) then
                begin
                  LReservoirVolume := AReservoir.ReservoirConfigurationData.VolumeWhenFull;
                  if LReservoirVolume > 0 then
                    LMonthlyValue    := LMonthlyValue/LReservoirVolume * 100.0;
                end
                else
                  if LMonthlyValue > 0 then
                    LMonthlyValue := LMonthlyValue/LMonthlyValue * 100.0;
              end;

              LTotal :=  LTotal + (LMonthlyValue * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LCount]);

              LAvarages [LCount] := LAvarages[LCount] + LMonthlyValue;
              ViewDialog.grdDataGrid.Cells[LCount,LIndex+1] := FormatOutputValue(FCurrentViewData,LMonthlyValue);
            end;
            LTotal := LTotal / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;
            ViewDialog.grdDataGrid.Cells[13,LIndex+1] := FormatOutputValue(FCurrentViewData,LTotal);
          end;
          LGrandAvarage := 0.00;
          for LIndex := Low(LAvarages)+1 to High(LAvarages) do
          begin
            LAvarages[LIndex] := LAvarages[LIndex] / AData.Count;
            ViewDialog.grdDataGrid.Cells[LIndex,ViewDialog.grdDataGrid.RowCount-1] :=  FormatOutputValue(FCurrentViewData,LAvarages[LIndex]);
            LGrandAvarage := LGrandAvarage+ (LAvarages[LIndex]  * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LIndex]);
          end;
          LGrandAvarage := LGrandAvarage / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear;
          ViewDialog.grdDataGrid.Cells[13,ViewDialog.grdDataGrid.RowCount-1] := FormatOutputValue(FCurrentViewData,LGrandAvarage);
          ViewDialog.grdDataGrid.Cells[0,ViewDialog.grdDataGrid.RowCount-1] := FAppModules.Language.GetString('MonthGrid.Average');
        finally
          LMonthlyValues.Free;
        end
      end;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

