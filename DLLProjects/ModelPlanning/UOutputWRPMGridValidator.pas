unit UOutputWRPMGridValidator;

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
  Messages,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UOutputWRPMGridDialog;

type
  TOutputWRPMGridValidator = class(TAbstractOutputDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnBtnDataSelectionClick(Sender: TObject);
    procedure OnViewDataTypeChange(Sender: TObject);

    procedure PopulateDialogSelectors;
    procedure RePopulateDataViewer;
    procedure PopulatePltGrid(AData : TStringList);
    procedure PopulateResGrid(AData : TStringList);
    procedure PopulateSysGrid(AData : TStringList;AColumnName: string);
    procedure PopulateInterBasinSupportGrid(AData : TStringList);
    procedure PopulatePmpGrid(AData : TStringList);
    function ViewDialog: TOutputWRPMGridDialog;
  public
    function Initialise: Boolean; override;
    function LanguageHasChanged: boolean; override;
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
  Windows,
  SysUtils,
  DateUtils,
  Variants,
  UConstants,
  UWRPMOutputSettings,
  URunConfigurationData,
  UPlanningModelDataObject,
  UAbstractFileNamesObject,
  UErrorHandlingOperations;

{ TOutputWRPMGridValidator }

procedure TOutputWRPMGridValidator.CreateMemberObjects;
const OPNAME = 'TOutputWRPMGridValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputWRPMGridDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.DestroyMemberObjects;
const OPNAME = 'TOutputWRPMGridValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMGridValidator.Initialise: Boolean;
const OPNAME = 'TOutputWRPMGridValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    PopulateDialogSelectors;
    ViewDialog.BtnDataSelection.OnClick := OnBtnDataSelectionClick;
    ViewDialog.cmbViewDataType.OnSelect := OnViewDataTypeChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMGridValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputWRPMGridValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'HYDROSEQCOUNT')  or
      (UpperCase(AFieldName) = 'OUTPUTDATASELECTION')then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMGridValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputWRPMGridValidator.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    TabShetCaption := 'Grid';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMGridValidator.ViewDialog : TOutputWRPMGridDialog;
const OPNAME = 'TOutputWRPMGridValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputWRPMGridDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.PopulateDialogSelectors;
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

procedure TOutputWRPMGridValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputWRPMGridValidator.OnViewDataTypeChange';
begin
  try
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.OnBtnDataSelectionClick(Sender: TObject);
const OPNAME = 'TOutputWRPMGridValidator.OnBtnDataSelectionClick';
begin
  try
    TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.ShowDataSelectionDialog(FIdentifier,FNetworkElementType,osdWRPMGrid,btNone,ovtNone);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.PopulateDataViewer;
const OPNAME = 'TOutputWRPMGridValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.ClearDataViewer;
const OPNAME = 'TOutputWRPMGridValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ViewDialog.grdDataGrid.RowCount := 2;
    ViewDialog.grdDataGrid.Rows[0].Clear;
    ViewDialog.grdDataGrid.Rows[1].Clear;
    ViewDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.RePopulateDataViewer;
const OPNAME = 'TOutputWRPMGridValidator.RePopulateDataViewer';
var
  LModelData  : TPlanningModelDataObject;
  LFileName   : string;
  LLineData   : TStringList;
  LOutputData : TStringList;
begin
  try
    LLineData   := TStringList.Create;
    LOutputData   := TStringList.Create;
    try
      LModelData       := TPlanningModelDataObject(FAppModules.Model.ModelData);

      case NetworkElementType of
        votMasterControl,votReviewSubSystemStorage,votReviewDamStorage:
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
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputWRPMGridValidator.PopulatePltGrid(AData: TStringList);
const OPNAME = 'TOutputWRPMGridValidator.PopulatePltGrid';
var
  LIndex          : integer;
  LRowIndex       : integer;
  LColIndex       : integer;
  LModelData      : TPlanningModelDataObject;
  LLineData       : TStringList;
  LOutputTimeStep : TOutputTimeStep;
  LPltFileHeaderData : TWRPMPltFileHeaderData;
begin
  try
    LLineData   := TStringList.Create;
    ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 0, 0);;
    try
      LOutputTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
      if(LOutputTimeStep in [otsMonthly,otsMonthlyCumulative]) then
      begin
        ViewDialog.grdDataGrid.RowCount   := 2;
        ViewDialog.grdDataGrid.ColCount   := 14;
        ViewDialog.grdDataGrid.FixedCols  := 2;
        ViewDialog.grdDataGrid.Rows[0].Clear;
        ViewDialog.grdDataGrid.Rows[1].Clear;

        LModelData := TPlanningModelDataObject(FAppModules.Model.ModelData);
        ViewDialog.grdDataGrid.RowCount := Trunc(AData.Count/12)+ 1;
        ViewDialog.grdDataGrid.Cells[0,0] := 'Sequence';
        ViewDialog.grdDataGrid.Cells[1,0] := 'Year';
        for LColIndex := 1 to 12 do
          ViewDialog.grdDataGrid.Cells[LColIndex+1,0] := LModelData.GetRunConfigurationData.Get_MonthNameByIndex(LColIndex);

        LColIndex := 1;
        LRowIndex := 1;
        for LIndex := 0 to AData.Count-1 do
        begin
          LLineData.CommaText := AData[LIndex];
          if(LColIndex = 1) then
          begin
            ViewDialog.grdDataGrid.Cells[0,LRowIndex] := LLineData[0];
            ViewDialog.grdDataGrid.Cells[1,LRowIndex] := IntToStr(YearOf(StrToDate(LLineData[1])));
          end;
          ViewDialog.grdDataGrid.Cells[LColIndex+1,LRowIndex] := LLineData[2];

          LColIndex := LColIndex + 1;
          if(LColIndex > 12) then
          begin
            LColIndex := 1;
            LRowIndex := LRowIndex + 1;
          end;
        end;
      end
      else if(LOutputTimeStep in [otsAnnual,otsAnnualCumulative]) then
      begin
        LModelData         := TPlanningModelDataObject(FAppModules.Model.ModelData);
        LPltFileHeaderData := LModelData.CastWRPMPostProcessorData.Get_PlotFileHeaderData;
        ViewDialog.grdDataGrid.RowCount   := 2;
        ViewDialog.grdDataGrid.ColCount   := LPltFileHeaderData.SequencesCount+1;
        ViewDialog.grdDataGrid.RowCount   := (LPltFileHeaderData.MonthsCount div 12) + 1;
        ViewDialog.grdDataGrid.FixedCols  := 1;
        ViewDialog.grdDataGrid.Rows[0].Clear;
        ViewDialog.grdDataGrid.Rows[1].Clear;

        ViewDialog.grdDataGrid.Cells[0,0] := 'Year';
        for LColIndex := 0 to LPltFileHeaderData.SequencesList.Count-1 do
          ViewDialog.grdDataGrid.Cells[LColIndex+1,0] := LPltFileHeaderData.SequencesList[LColIndex];

        LColIndex := 1;
        LRowIndex := 1;
        AData.Sort;
        for LIndex := 0 to AData.Count-1 do
        begin
          LLineData.CommaText := AData[LIndex];
          if(LColIndex = 1) then
            ViewDialog.grdDataGrid.Cells[0,LRowIndex] := IntToStr(YearOf(StrToDate(LLineData[1])));
          ViewDialog.grdDataGrid.Cells[LColIndex,LRowIndex] := LLineData[2];

          LColIndex := LColIndex + 1;
          if(LColIndex > LPltFileHeaderData.SequencesCount) then
          begin
            LColIndex := 1;
            LRowIndex := LRowIndex + 1;
          end;
        end;
      end;
    finally
      ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 1, 0);;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.PopulateResGrid(AData: TStringList);
const OPNAME = 'TOutputWRPMGridValidator.PopulateResGrid';
var
  LIndex          : integer;
  LColIndex       : integer;
  LLineData       : TStringList;
begin
  try
    LLineData   := TStringList.Create;
    ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 0, 0);;
    try
      ViewDialog.grdDataGrid.RowCount   := AData.Count+1;
      ViewDialog.grdDataGrid.ColCount   := 5;
      ViewDialog.grdDataGrid.FixedCols  := 2;
      ViewDialog.grdDataGrid.Rows[0].Clear;
      ViewDialog.grdDataGrid.Rows[1].Clear;

      ViewDialog.grdDataGrid.Cells[0,0] := 'Sequence';
      ViewDialog.grdDataGrid.Cells[1,0] := 'Year';
      ViewDialog.grdDataGrid.Cells[2,0] := 'Demand';
      ViewDialog.grdDataGrid.Cells[3,0] := 'Allocation';
      ViewDialog.grdDataGrid.Cells[4,0] := 'Supply';

      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        for LColIndex := 0 to LLineData.Count-1 do
          ViewDialog.grdDataGrid.Cells[LColIndex,LIndex+1] := LLineData[LColIndex];
      end;
    finally
      ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 1, 0);;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.PopulateInterBasinSupportGrid(AData: TStringList);
const OPNAME = 'TOutputWRPMGridValidator.PopulateResGrid';
var
  LIndex          : integer;
  LColIndex       : integer;
  LLineData       : TStringList;
begin
  try
    LLineData   := TStringList.Create;
    ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 0, 0);;
    try
      ViewDialog.grdDataGrid.RowCount   := AData.Count+1;
      ViewDialog.grdDataGrid.ColCount   := 3;
      ViewDialog.grdDataGrid.FixedCols  := 2;
      ViewDialog.grdDataGrid.Rows[0].Clear;
      ViewDialog.grdDataGrid.Rows[1].Clear;

      ViewDialog.grdDataGrid.Cells[0,0] := 'Sequence';
      ViewDialog.grdDataGrid.Cells[1,0] := 'Year';
      ViewDialog.grdDataGrid.Cells[2,0] := 'Volume';

      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        for LColIndex := 0 to LLineData.Count-1 do
          ViewDialog.grdDataGrid.Cells[LColIndex,LIndex+1] := LLineData[LColIndex];
      end;
    finally
      ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 1, 0);;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.PopulateSysGrid(AData: TStringList;AColumnName: string);
const OPNAME = 'TOutputWRPMGridValidator.PopulateResGrid';
var
  LIndex          : integer;
  LColIndex       : integer;
  LLineData       : TStringList;
begin
  try
    LLineData   := TStringList.Create;
    ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 0, 0);;
    try
      ViewDialog.grdDataGrid.RowCount   := AData.Count+1;
      ViewDialog.grdDataGrid.ColCount   := 3;
      ViewDialog.grdDataGrid.FixedCols  := 2;
      ViewDialog.grdDataGrid.Rows[0].Clear;
      ViewDialog.grdDataGrid.Rows[1].Clear;

      ViewDialog.grdDataGrid.Cells[0,0] := 'Sequence';
      ViewDialog.grdDataGrid.Cells[1,0] := 'Year';
      ViewDialog.grdDataGrid.Cells[2,0] := AColumnName;

      for LIndex := 0 to AData.Count-1 do
      begin
        LLineData.CommaText := AData[LIndex];
        for LColIndex := 0 to LLineData.Count-1 do
          ViewDialog.grdDataGrid.Cells[LColIndex,LIndex+1] := LLineData[LColIndex];
      end;
    finally
      ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 1, 0);;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.PopulatePmpGrid(AData: TStringList);
const OPNAME = 'TOutputWRPMGridValidator.PopulatePmpGrid';
var
  LIndex          : integer;
  LRowIndex       : integer;
  LColIndex       : integer;
  LLineData       : TStringList;
  LModelData      : TPlanningModelDataObject;
  LOutputTimeStep : TOutputTimeStep;
begin
  try
    LLineData   := TStringList.Create;
    ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 0, 0);;
    try
      LOutputTimeStep := TPlanningModelDataObject(FAppModules.Model.ModelData).OutputData.CastDataSelection.TimeStep;
      if(LOutputTimeStep in [otsMonthly,otsMonthlyCumulative]) then
      begin
        {ViewDialog.grdDataGrid.ColCount   := 3;
        ViewDialog.grdDataGrid.FixedCols  := 2;
        ViewDialog.grdDataGrid.Rows[0].Clear;
        ViewDialog.grdDataGrid.Rows[1].Clear;

        ViewDialog.grdDataGrid.Cells[0,0] := 'Sequence';
        ViewDialog.grdDataGrid.Cells[1,0] := 'Date';
        ViewDialog.grdDataGrid.Cells[2,0] := 'Flow(m³/s)';

        for LIndex := 0 to AData.Count-1 do
        begin
          LLineData.CommaText := AData[LIndex];
          for LColIndex := 0 to LLineData.Count-1 do
            ViewDialog.grdDataGrid.Cells[LColIndex,LIndex+1] := LLineData[LColIndex];
        end;}
        ViewDialog.grdDataGrid.RowCount   := 2;
        ViewDialog.grdDataGrid.ColCount   := 14;
        ViewDialog.grdDataGrid.FixedCols  := 2;
        ViewDialog.grdDataGrid.Rows[0].Clear;
        ViewDialog.grdDataGrid.Rows[1].Clear;

        LModelData := TPlanningModelDataObject(FAppModules.Model.ModelData);
        ViewDialog.grdDataGrid.RowCount := Trunc(AData.Count/12)+ 1;
        ViewDialog.grdDataGrid.Cells[0,0] := 'Sequence';
        ViewDialog.grdDataGrid.Cells[1,0] := 'Year';
        for LColIndex := 1 to 12 do
          ViewDialog.grdDataGrid.Cells[LColIndex+1,0] := LModelData.GetRunConfigurationData.Get_MonthNameByIndex(LColIndex);

        LColIndex := 1;
        LRowIndex := 1;
        for LIndex := 0 to AData.Count-1 do
        begin
          LLineData.CommaText := AData[LIndex];
          if(LColIndex = 1) then
          begin
            ViewDialog.grdDataGrid.Cells[0,LRowIndex] := LLineData[0];
            ViewDialog.grdDataGrid.Cells[1,LRowIndex] := IntToStr(YearOf(StrToDate(LLineData[1])));
        end;
          ViewDialog.grdDataGrid.Cells[LColIndex+1,LRowIndex] := LLineData[2];

          LColIndex := LColIndex + 1;
          if(LColIndex > 12) then
          begin
            LColIndex := 1;
            LRowIndex := LRowIndex + 1;
          end;
        end;
      end
      else if(LOutputTimeStep in [otsAnnual,otsAnnualCumulative]) then
      begin
       ViewDialog.grdDataGrid.RowCount   := AData.Count+1;
        ViewDialog.grdDataGrid.ColCount   := 2;
        ViewDialog.grdDataGrid.FixedCols  := 1;
        ViewDialog.grdDataGrid.Rows[0].Clear;
        ViewDialog.grdDataGrid.Rows[1].Clear;

        ViewDialog.grdDataGrid.Cells[0,0] := 'Year';
        ViewDialog.grdDataGrid.Cells[1,0] := 'Flow(m³/a)';

        for LIndex := 0 to AData.Count-1 do
        begin
          LLineData.CommaText := AData[LIndex];
          ViewDialog.grdDataGrid.Cells[0,LIndex+1] := IntToStr(YearOf(StrToDate(LLineData[1])));
          ViewDialog.grdDataGrid.Cells[1,LIndex+1] := LLineData[2];
        end;
      end
    finally
      ViewDialog.grdDataGrid.Perform(WM_SETREDRAW, 1, 0);;
      LLineData.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMGridValidator.CanExport: boolean;
const OPNAME = 'TOutputWRPMGridValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputWRPMGridValidator.CanPrint: boolean;
const OPNAME = 'TOutputWRPMGridValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputWRPMGridValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWRPMGridValidator.DoPrint;
const OPNAME = 'TOutputWRPMGridValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

