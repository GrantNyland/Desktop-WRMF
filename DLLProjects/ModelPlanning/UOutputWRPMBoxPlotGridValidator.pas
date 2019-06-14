
unit UOutputWRPMBoxPlotGridValidator;

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
  TOutputWRPMBoxPlotGridValidator = class(TAbstractOutputDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure OnViewDataTypeChange(Sender: TObject);

    procedure PopulateDialogSelectors;
    procedure RePopulateDataViewer;
    procedure PopulatePltGrid(AData : TStrings);
    procedure PopulateResGrid(AData : TStrings);
    procedure PopulateSysGrid(AData : TStringList;AColumnName: string);
    procedure PopulateInterBasinSupportGrid(AData : TStrings);
    procedure PopulatePmpGrid(AData : TStrings);
    function ViewDialog: TOutputGridDialog;
  public
    function Initialise: Boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    function CanExport : boolean; override;
    function CanPrint  : boolean; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
  end;

implementation

uses
  Math,
  DateUtils,
  Variants,
  //Series,
  Windows,
  SysUtils,
  UConstants,
  UOutputData,
  UDataSetType,
  UBoxPlotData,
  UWRPMOutputSettings,
  UPlanningModelDataObject,
  UWRPMPostProcessorData,
  UAbstractFileNamesObject,
  UErrorHandlingOperations;

{ TOutputWRPMBoxPlotGridValidator }

procedure TOutputWRPMBoxPlotGridValidator.CreateMemberObjects;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputGridDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.DestroyMemberObjects;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGridValidator.Initialise: Boolean;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ViewDialog.cmbViewDataType.OnSelect := OnViewDataTypeChange;
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGridValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'HYDROSEQCOUNT')  or
      (UpperCase(AFieldName) = 'OUTPUTDATASELECTION')then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGridValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGridValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    TabShetCaption := 'Box Plot Data';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGridValidator.ViewDialog : TOutputGridDialog;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputGridDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.PopulateDialogSelectors;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.PopulateDialogSelectors';
var
  LIndex       : integer;
  LName        : string;
  LSysPrefix   : string;
  LFileName    : TAbstractModelFileName;
  LModelData   : TPlanningModelDataObject;
begin
  try
    ViewDialog.BtnDataSelection.Enabled := True;
    ViewDialog.cmbViewDataType.Visible  := False;
    ViewDialog.ViewDataLabel.Visible    := False;
    ViewDialog.cmbViewDataType.Clear;

    case NetworkElementType of
      votMasterControl,votReviewSubSystemStorage,votReviewDamStorage:
      begin
      end;
      votReviewTotalSystemStorage:
      begin
      end;
      votReviewSubSystemCurtailment:
      begin
      end;
      votTotalSystemCurtailment:
      begin
      end;
      votReviewDemandSupply:
      begin
      end;
      votReviewInterBasinSupport:
      begin
        LModelData := TPlanningModelDataObject(FAppModules.Model.ModelData);
        LSysPrefix := UpperCase(LModelData.DataFilePath.DataFilePrefix)+ 'RES';
        for LIndex := 0 to LModelData.FileNamesObject.OutputFileNames.Count - 1 do
        begin
          LFileName := LModelData.FileNamesObject.OutputFileNames.FileNameObject[LIndex];
          if Assigned(LFileName) and FileExists(LFileName.FileName) then
          begin
            LName := UpperCase(ExtractFileName(LFileName.ShortName));
            if(Pos(LSysPrefix,LName) > 0) then
              ViewDialog.cmbViewDataType.Items.Add(LName);
          end;
        end;
        ViewDialog.cmbViewDataType.Visible   := True;
        ViewDialog.ViewDataLabel.Visible     := True;
        ViewDialog.cmbViewDataType.ItemIndex := 0;
      end;
      votChannel:
      begin
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.OnViewDataTypeChange';
begin
  try
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.OnBtnDataSelectionClick';
begin
  try
    TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdWRPMBoxPlotGrid,btNone,ovtNone);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.PopulateDataViewer;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    PopulateDialogSelectors;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.ClearDataViewer;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.ClearDataViewer';
var
  LIndex: integer;
begin
  inherited ClearDataViewer;
  try
    ViewDialog.grdDataGrid.RowCount := 2;
    for LIndex := 0 to ViewDialog.grdDataGrid.ColCount -1 do
      ViewDialog.grdDataGrid.Cols[LIndex].Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputWRPMBoxPlotGridValidator.RePopulateDataViewer;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.RePopulateDataViewer';
var
  LFileName   : string;
  LModelData  : TPlanningModelDataObject;
  LOutputData : TStringList;
begin
  try
    LOutputData := TStringList.Create;
    try
      LModelData := TPlanningModelDataObject(FAppModules.Model.ModelData);
      case NetworkElementType of
        votMasterControl,votReviewDamStorage,votReviewSubSystemStorage:
        begin
          if LModelData.CastWRPMPostProcessorData.GetPltFileElementData(LOutputData,FIdentifier,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulatePltGrid(LOutputData);
          end;
        end;
        votReviewTotalSystemStorage:
        begin
          if LModelData.CastWRPMPostProcessorData.GetSysFileElementData(LOutputData,FIdentifier,ofcnSystemStorage,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateSysGrid(LOutputData,ofcnSystemStorage);
          end;
        end;
        votReviewSubSystemCurtailment:
        begin
          LFileName := LModelData.CastWRPMPostProcessorData.GetSubSystemSysFileName(FElementName);
          if LModelData.CastWRPMPostProcessorData.GetSysFileElementData(LOutputData,FIdentifier,FElementName,LFileName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateSysGrid(LOutputData,FElementName);
          end;
        end;
        votTotalSystemCurtailment:
        begin
          if LModelData.CastWRPMPostProcessorData.GetSysFileElementData(LOutputData,FIdentifier,ofcnSystemCurtailment,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateSysGrid(LOutputData,ofcnSystemCurtailment);
          end;
        end;
        votReviewDemandSupply:
        begin
          if LModelData.CastWRPMPostProcessorData.GetResFileElementData(LOutputData,FIdentifier,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateResGrid(LOutputData);
          end;
        end;
        votReviewInterBasinSupport:
        begin
          LFileName := ViewDialog.cmbViewDataType.Text;
          if LModelData.CastWRPMPostProcessorData.GetInterBasinSupportFileElementData(LOutputData,FIdentifier,FElementName,LFileName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulateInterBasinSupportGrid(LOutputData);
          end;
        end;
        votChannel:
        begin
          if LModelData.CastWRPMPostProcessorData.GetPmpFileElementData(LOutputData,FIdentifier,FElementName,NetworkElementType) then
          begin
            if(LOutputData.Count > 0) then
              PopulatePmpGrid(LOutputData);
          end;
        end;
      end;
    finally
      LOutputData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.PopulatePltGrid(AData: TStrings);
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.PopulatePltGrid';
var
  LColIndex        : integer;
  LIndex           : integer;
  LDate            : TDate;
  LLineData        : TStringList;
  LBoxPlotData     : TBoxPlotData;
  LBoxPlotDataList : TBoxPlotDataList;
  LOutputTimeStep  : TOutputTimeStep;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    try
      ViewDialog.grdDataGrid.RowCount   := 2;
      ViewDialog.grdDataGrid.Rows[0].Clear;
      ViewDialog.grdDataGrid.Rows[1].Clear;
      ViewDialog.grdDataGrid.ColCount   := 14;
      ViewDialog.grdDataGrid.FixedCols  := 1;

      LOutputTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
      if(LOutputTimeStep in [otsMonthly,otsMonthlyCumulative]) then
        ViewDialog.grdDataGrid.Cells[0,0] := 'Year/Month'
      else if(LOutputTimeStep in [otsAnnual,otsAnnualCumulative]) then
        ViewDialog.grdDataGrid.Cells[0,0] := 'Year';

      for LColIndex := Low(Pecentile13) to High(Pecentile13) do
        ViewDialog.grdDataGrid.Cells[LColIndex,0] := FormatFloat('0.0',Pecentile13[LColIndex])+'%';

      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LDate               := StrToDate(LLineData[1]);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);

        LBoxPlotData.AddValue(StrToFloat(LLineData[2]));
      end;

      ViewDialog.grdDataGrid.RowCount := LBoxPlotDataList.Count+ 1;
      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LDate         := LBoxPlotData.XValueDate;
        if(LOutputTimeStep in [otsMonthly,otsMonthlyCumulative]) then
          ViewDialog.grdDataGrid.Cells[00,LIndex+1] := IntToStr(YearOf(LDate)) + '/' + FormatFloat('00',MonthOf(LDate))
        else if(LOutputTimeStep in [otsAnnual,otsAnnualCumulative]) then
          ViewDialog.grdDataGrid.Cells[00,LIndex+1] := IntToStr(YearOf(LDate));
        for LColIndex := Low(Pecentile13) to High(Pecentile13) do
          ViewDialog.grdDataGrid.Cells[LColIndex,LIndex+1] := FormatFloat('0.0000',LBoxPlotData.PercValue(Pecentile13[LColIndex]));
      end;
    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.PopulateResGrid(AData: TStrings);
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.PopulateResGrid';
var
  LYear,LMonth     : integer;
  LColIndex        : integer;
  LIndex           : integer;
  LDate            : TDate;
  LLineData        : TStringList;
  LBoxPlotData     : TBoxPlotData;
  LBoxPlotDataList : TBoxPlotDataList;
  LValueType       : TOutputValueType;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    try
      ViewDialog.grdDataGrid.RowCount   := 2;
      ViewDialog.grdDataGrid.Rows[0].Clear;
      ViewDialog.grdDataGrid.Rows[1].Clear;
      ViewDialog.grdDataGrid.ColCount   := 14;
      ViewDialog.grdDataGrid.FixedCols  := 1;

      ViewDialog.grdDataGrid.Cells[0,0] := 'Year';

      for LColIndex := Low(Pecentile13) to High(Pecentile13) do
        ViewDialog.grdDataGrid.Cells[LColIndex,0] := FormatFloat('0.0',Pecentile13[LColIndex])+'%';

      LMonth      := FAppModules.StudyArea.CalendarStartMonth;
      LValueType  := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.Get_GetSelection.ValueType;
      LColIndex   := 4;
      case LValueType of
        ovtDemand    : LColIndex   := 2;
        ovtAllocated : LColIndex   := 3;
        ovtSupply    : LColIndex   := 4;
      end;

      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LYear               := StrToInt(LLineData[1]);
        LDate               := EncodeDate(LYear,LMonth,01);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);

        LBoxPlotData.AddValue(StrToFloat(LLineData[LColIndex]));
      end;

      ViewDialog.grdDataGrid.RowCount := LBoxPlotDataList.Count+ 1;
      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LDate         := LBoxPlotData.XValueDate;
        ViewDialog.grdDataGrid.Cells[00,LIndex+1] := IntToStr(YearOf(LDate));
        for LColIndex := Low(Pecentile13) to High(Pecentile13) do
          ViewDialog.grdDataGrid.Cells[LColIndex,LIndex+1] := FormatFloat('0.0000',LBoxPlotData.PercValue(Pecentile13[LColIndex]));
      end;
    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.PopulateInterBasinSupportGrid(AData: TStrings);
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.PopulateResGrid';
var
  LYear,LMonth     : integer;
  LColIndex        : integer;
  LIndex           : integer;
  LDate            : TDate;
  LLineData        : TStringList;
  LBoxPlotData     : TBoxPlotData;
  LBoxPlotDataList : TBoxPlotDataList;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    try
      ViewDialog.grdDataGrid.RowCount   := 2;
      ViewDialog.grdDataGrid.Rows[0].Clear;
      ViewDialog.grdDataGrid.Rows[1].Clear;
      ViewDialog.grdDataGrid.ColCount   := 14;
      ViewDialog.grdDataGrid.FixedCols  := 1;

      ViewDialog.grdDataGrid.Cells[0,0] := 'Year';

      for LColIndex := Low(Pecentile13) to High(Pecentile13) do
        ViewDialog.grdDataGrid.Cells[LColIndex,0] := FormatFloat('0.0',Pecentile13[LColIndex])+'%';

      LMonth := FAppModules.StudyArea.CalendarStartMonth;
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LYear               := StrToInt(LLineData[1]);
        LDate               := EncodeDate(LYear,LMonth,01);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);

        LBoxPlotData.AddValue(StrToFloat(LLineData[2]));
      end;

      ViewDialog.grdDataGrid.RowCount := LBoxPlotDataList.Count+ 1;
      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LDate         := LBoxPlotData.XValueDate;
        ViewDialog.grdDataGrid.Cells[00,LIndex+1] := IntToStr(YearOf(LDate));
        for LColIndex := Low(Pecentile13) to High(Pecentile13) do
          ViewDialog.grdDataGrid.Cells[LColIndex,LIndex+1] := FormatFloat('0.0000',LBoxPlotData.PercValue(Pecentile13[LColIndex]));
      end;
    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.PopulatePmpGrid(AData: TStrings);
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.PopulatePmpGrid';
var
  LColIndex        : integer;
  LIndex           : integer;
  LDate            : TDate;
  LLineData        : TStringList;
  LBoxPlotData     : TBoxPlotData;
  LBoxPlotDataList : TBoxPlotDataList;
  LOutputTimeStep  : TOutputTimeStep;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    try
      ViewDialog.grdDataGrid.RowCount   := 2;
      ViewDialog.grdDataGrid.Rows[0].Clear;
      ViewDialog.grdDataGrid.Rows[1].Clear;
      ViewDialog.grdDataGrid.ColCount   := 14;
      ViewDialog.grdDataGrid.FixedCols  := 1;

      LOutputTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
      if(LOutputTimeStep in [otsMonthly,otsMonthlyCumulative]) then
        ViewDialog.grdDataGrid.Cells[0,0] := 'Year/Month'
      else if(LOutputTimeStep in [otsAnnual,otsAnnualCumulative]) then
        ViewDialog.grdDataGrid.Cells[0,0] := 'Year';

      for LColIndex := Low(Pecentile13) to High(Pecentile13) do
        ViewDialog.grdDataGrid.Cells[LColIndex,0] := FormatFloat('0.0',Pecentile13[LColIndex])+'%';

      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LDate               := StrToDate(LLineData[1]);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);

        LBoxPlotData.AddValue(StrToFloat(LLineData[2]));
      end;

      ViewDialog.grdDataGrid.RowCount := LBoxPlotDataList.Count+ 1;
      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LDate         := LBoxPlotData.XValueDate;
        if(LOutputTimeStep in [otsMonthly,otsMonthlyCumulative]) then
          ViewDialog.grdDataGrid.Cells[00,LIndex+1] := IntToStr(YearOf(LDate)) + '/' + FormatFloat('00',MonthOf(LDate))
        else if(LOutputTimeStep in [otsAnnual,otsAnnualCumulative]) then
          ViewDialog.grdDataGrid.Cells[00,LIndex+1] := IntToStr(YearOf(LDate));
        for LColIndex := Low(Pecentile13) to High(Pecentile13) do
          ViewDialog.grdDataGrid.Cells[LColIndex,LIndex+1] := FormatFloat('0.0000',LBoxPlotData.PercValue(Pecentile13[LColIndex]));
      end;
    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGridValidator.CanExport: boolean;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMBoxPlotGridValidator.CanPrint: boolean;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.DoPrint;
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMBoxPlotGridValidator.PopulateSysGrid(AData: TStringList;AColumnName: string);
const OPNAME = 'TOutputWRPMBoxPlotGridValidator.PopulateSysGrid';
var
  LYear,LMonth     : integer;
  LColIndex        : integer;
  LIndex           : integer;
  LDate            : TDate;
  LLineData        : TStringList;
  LBoxPlotData     : TBoxPlotData;
  LBoxPlotDataList : TBoxPlotDataList;
begin
  try
    LLineData        := TStringList.Create;
    LBoxPlotDataList := TBoxPlotDataList.Create;
    try
      ViewDialog.grdDataGrid.RowCount   := 2;
      ViewDialog.grdDataGrid.Rows[0].Clear;
      ViewDialog.grdDataGrid.Rows[1].Clear;
      ViewDialog.grdDataGrid.ColCount   := 14;
      ViewDialog.grdDataGrid.FixedCols  := 1;

      ViewDialog.grdDataGrid.Cells[0,0] := 'Year';

      for LColIndex := Low(Pecentile13) to High(Pecentile13) do
        ViewDialog.grdDataGrid.Cells[LColIndex,0] := FormatFloat('0.0',Pecentile13[LColIndex])+'%';

      LMonth := FAppModules.StudyArea.CalendarStartMonth;
      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        LYear               := StrToInt(LLineData[1]);
        LDate               := EncodeDate(LYear,LMonth,01);
        LBoxPlotData        := LBoxPlotDataList.BoxPlotDataByXValue[LDate];
        if(LBoxPlotData = nil) then
          LBoxPlotData        := LBoxPlotDataList.CreateBoxPlotData(LDate);

        LBoxPlotData.AddValue(StrToFloat(LLineData[2]));
      end;

      ViewDialog.grdDataGrid.RowCount := LBoxPlotDataList.Count+ 1;
      for LIndex := 0 to LBoxPlotDataList.Count-1 do
      begin
        LBoxPlotData  := LBoxPlotDataList.BoxPlotDataByIndex[LIndex];
        LBoxPlotData.SortData;
        LDate         := LBoxPlotData.XValueDate;
        ViewDialog.grdDataGrid.Cells[00,LIndex+1] := IntToStr(YearOf(LDate));
        for LColIndex := Low(Pecentile13) to High(Pecentile13) do
          ViewDialog.grdDataGrid.Cells[LColIndex,LIndex+1] := FormatFloat('0.0000',LBoxPlotData.PercValue(Pecentile13[LColIndex]));
      end;
    finally
      LLineData.Free;
      LBoxPlotDataList.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

