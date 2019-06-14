
unit UOutputDemandChannelsGridSummaryValidator;

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
  ULongtermSupplyData,
  UOutputDemandChannelsGridSummaryDialog;

type

  TComplianceArray = array of double;

  TOutputDemandChannelsGridSummaryValidator = class(TAbstractOutputDialogValidator)
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
    procedure DestroyMemberObjects; override;

    procedure OnViewDataTypeChange(Sender: TObject);
    procedure DoClickChannels(Sender: TObject);

    procedure RePopulateDataViewer;
    procedure PopulateGridHeaders;
    function PopulateTableData(AChannelNumber : integer;ARow : integer): boolean ;
    procedure DeleteTableData(AChannelNumber : integer);
    procedure ClearGrid;
    procedure GetSelectionData;
    procedure ChangeViewDataLabel;
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
    function ViewDialog: TOutputDemandChannelsGridSummaryDialog;overload;
  end;

implementation

uses
  VCLTee.Series,
  Windows,
  SysUtils,
  VCL.Graphics,
  VCL.Dialogs,
  VCL.Forms,
  UConstants,
  UOutputData,
  UDataSetType,
  UOutputGridValidator,
  UYieldModelDataObject,
  UErrorHandlingOperations, Math, URunConfigurationData, VCL.Grids;

{ TOutputDemandChannelsGridSummaryValidator }

procedure TOutputDemandChannelsGridSummaryValidator.CreateMemberObjects;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := TOutputDemandChannelsGridSummaryDialog.Create(FPanelOwner,FAppModules);

    ViewDialog.lsvDemandChannels.OnClick := DoClickChannels;
    ViewDialog.ViewDataType.OnSelect := OnViewDataTypeChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.DestroyMemberObjects;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.CreateMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryValidator.Initialise: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.Initialise';
var
  LIndex : integer;
  LRunConfig : IRunConfigurationData;
begin
  Result := inherited Initialise;
  try
    FCurrentViewData := btNone;
    FPrevViewData    := btNone;
    LRunConfig := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData;
    if LRunConfig <> nil then
    begin
      for LIndex := 1 to LRunConfig.NrOfActiveLoadCases do
      begin
        ViewDialog.ViewDataType.Items.AddObject('Target Draft '+IntToStr(LIndex)+' ('+
        FloatToStr(LRunConfig.TargetYieldByIndex[LIndex])+')',TObject(LIndex));
      end;
    end;

    if (ViewDialog.ViewDataType.ItemIndex < 0) then
      ViewDialog.ViewDataType.ItemIndex := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.OnViewDataTypeChange(Sender: TObject);
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.OnViewDataTypeChange';
begin
  try
    if (ViewDialog.ViewDataType.ItemIndex < 0) then
      ViewDialog.ViewDataType.ItemIndex := 0;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOutputDemandChannelsGridSummaryValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (UpperCase(AFieldName) = 'LOADCASESCOUNT') OR
       (UpperCase(AFieldName) = 'HYDROSEQCOUNT')  OR
       (UpperCase(AFieldName) = 'OUTPUTDATASELECTION') then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    SaveState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.OutputDemandChannelSummaryGrid');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryValidator.SaveState: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryValidator.ViewDialog : TOutputDemandChannelsGridSummaryDialog;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.ViewDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputDemandChannelsGridSummaryDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryValidator.GetNextSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.GetNextSignificantRecord';
begin
  Result := 0;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryValidator.GetPreviousSignificantRecord(ASender: TObject;ACurrentRecord: integer): integer;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.GetPreviousSignificantRecord';
begin
  Result := 0;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.ClearDataViewer;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearGrid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.PopulateDataViewer;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.PopulateGridHeaders;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.PopulateGridHeaders';
var
  LIndex : integer;
  LColCount : integer;
begin
  try
    LColCount := 2;
    if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData <> nil then
    begin
      ViewDialog.sgrdDemandChannels.ColCount := LColCount+(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.FPrioritySplitStr.Count*2)+1;
      ViewDialog.sgrdDemandChannels.Cells[0,0] := 'Channel';
      ViewDialog.sgrdDemandChannels.Cells[1,0] := 'ELU';
      ViewDialog.sgrdDemandChannels.RowHeights[0] := 35;
      for LIndex := 0 to TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.FPrioritySplitStr.Count-1 do
      begin
        ViewDialog.sgrdDemandChannels.Cells[LIndex+2,0] := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.FPrioritySplitStr[LIndex];
        LColCount := LColCount+1;
      end;
      LIndex := 0;
      while LColCount<ViewDialog.sgrdDemandChannels.ColCount-1 do
      begin
        ViewDialog.sgrdDemandChannels.Cells[LColCount,0] := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.FDemandSplitstr[LIndex];
        LColCount := LColCount+1;
        LIndex := LIndex+1;
      end;
      ViewDialog.sgrdDemandChannels.Cells[ViewDialog.sgrdDemandChannels.ColCount-1,0] := 'Total Demand';
    end;
    ViewDialog.sgrdChannelSummary.RowHeights[0] := 55;
    ViewDialog.sgrdChannelSummary.ColWidths[0] := 45;
    ViewDialog.sgrdChannelSummary.Cells[0,0] := 'Channel No';
    ViewDialog.sgrdChannelSummary.ColWidths[1] := 150;
    ViewDialog.sgrdChannelSummary.Cells[1,0] := 'Channel Name';
    ViewDialog.sgrdChannelSummary.ColWidths[2] := 65;
    ViewDialog.sgrdChannelSummary.Cells[2,0] := 'Full Demand (mill m3/annual)';
    ViewDialog.sgrdChannelSummary.Cells[3,0] := 'No. Sequences Full Demand Obtained';
    ViewDialog.sgrdChannelSummary.ColWidths[4] := 98;
    ViewDialog.sgrdChannelSummary.Cells[4,0] := 'No. Sequences Supply Does Not Equalfull Demand';
    ViewDialog.sgrdChannelSummary.ColWidths[5] := 98;
    ViewDialog.sgrdChannelSummary.Cells[5,0] := 'Break Point Recurrence Interval(% of number sequences simulated)';
    ViewDialog.sgrdChannelSummary.ColWidths[6] := 98;
    ViewDialog.sgrdChannelSummary.Cells[6,0] := 'Supply Criteria Satisfactory(Yes/No)';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.RePopulateDataViewer;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.RePopulateDataViewer';
var
  LChannelList : IChannelList;
  LChannel : IGeneralFlowChannel;
  LIndex : integer;
  LChannelNumber : integer;

  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSelectedData : string;
  LSelectedValues : TStringList;
begin
  try
    LSelectedValues := TStringList.Create;
    try
      ClearGrid;
      GetSelectionData;
      PopulateGridHeaders;

      LModel := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','Model','');
      LStudyAreaName := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','StudyAreaName','');
      LSubArea := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','SubArea','');
      LScenario := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','Scenario','');
      if (FAppModules.StudyArea.ModelCode = LModel) and (FAppModules.StudyArea.StudyAreaCode = LStudyAreaName) and
       (FAppModules.StudyArea.SubAreaCode= LSubArea) and
       (FAppModules.StudyArea.ScenarioCode = LScenario) then
        LSelectedData := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','SelectedValues','');
        
      LSelectedValues.CommaText := LSelectedData;
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
      ViewDialog.lsvDemandChannels.Clear;
      for LIndex := 0 to LChannelList.ChannelCount-1 do
      begin
        LChannel := LChannelList.ChannelByIndex[LIndex];
        if LChannel <> nil then
        begin
          if LChannel.ChannelType in [ctMasterControlChannel,ctMinMaxChannel,
                                      ctDemandChannel,ctIrrigationBlockInflowChannel,
                                      ctIFRChannel] then
            ViewDialog.lsvDemandChannels.AddItem(LChannel.ChannelName,TObject(LChannel.ChannelNumber));
          if LSelectedValues.IndexOf(IntToStr(LChannel.ChannelNumber))>=0 then
            ViewDialog.lsvDemandChannels.Checked[ViewDialog.lsvDemandChannels.Items.IndexOf(LChannel.ChannelName)] := True;
        end;
      end;
      if LSelectedValues.Count>0 then
      begin
        ViewDialog.sgrdDemandChannels.RowCount := ViewDialog.sgrdDemandChannels.RowCount+LSelectedValues.Count-1;
        ViewDialog.sgrdChannelSummary.RowCount := ViewDialog.sgrdChannelSummary.RowCount+LSelectedValues.Count-1;
      end;
      for LIndex := 0 to LSelectedValues.Count-1 do
      begin
       if Trim(LSelectedValues[LIndex]) <> '' then
        begin
          LChannelNumber := StrToInt(LSelectedValues[LIndex]);
          PopulateTableData(LChannelNumber,LIndex+1);
        end;
      end;
    finally
      LSelectedValues.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TOutputDemandChannelsGridSummaryValidator.PopulateTableData(AChannelNumber : integer;ARow : integer): boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.PopulateTableData';
var
  LIndex : integer;
  LLongtermSupply : TLongtermSupply;
  LColCount : integer;
  LCount : integer;
  LTotal : double;
  LSelectedValues : TStringList;
begin
  Result := False;
  try
    if TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData <> nil then
    begin
      LColCount := 2;
      LLongtermSupply := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.GetLongtermSupplyByChannel(AChannelNumber,ViewDialog.ViewDataType.ItemIndex);
      if LLongtermSupply = nil then
      begin
        TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.CreateLongtermSupply(AChannelNumber);
        LLongtermSupply := TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.GetLongtermSupplyByChannel(AChannelNumber,ViewDialog.ViewDataType.ItemIndex);
      end;

      if (LLongtermSupply <> nil) then
      begin
        if (ARow = 2) and (ViewDialog.sgrdDemandChannels.RowCount = 3) and
           (Trim(ViewDialog.sgrdDemandChannels.cells[0,1]) = '') then
        begin
          ViewDialog.sgrdDemandChannels.RowCount := 2;
          ViewDialog.sgrdChannelSummary.RowCount := 2;
          PopulateGridHeaders;
          ARow := 1;
        end;

        if (ViewDialog.sgrdDemandChannels.ColCount = 3) and (TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastLongtermSupplyData.FPrioritySplitStr.Count>0) then
          PopulateGridHeaders;


        ViewDialog.sgrdDemandChannels.Cells[0,ARow] := IntToStr(AChannelNumber);
        ViewDialog.sgrdDemandChannels.Cells[1,ARow] := FormatFloat('0.0000',LLongtermSupply.ActualDemand);
        ViewDialog.sgrdDemandChannels.Alignment := taCenter;

        for LCount := 0 to High(LLongtermSupply.PrioritySplit) do
        begin
          ViewDialog.sgrdDemandChannels.Cells[LCount+2,ARow] :=  FormatFloat('0.000',LLongtermSupply.PrioritySplit[LCount+1]);
          LColCount := LColCount+1;
        end;

        LTotal := 0;

        for LCount := 0 to High(LLongtermSupply.DemandSplit) do
        begin
          ViewDialog.sgrdDemandChannels.Cells[LColCount-1,ARow] := FormatFloat('0.0000',LLongtermSupply.DemandSplit[LCount+1]);
          LColCount := LColCount+1;
          LTotal := LTotal +LLongtermSupply.DemandSplit[LCount+1];
        end;
        //if LLongtermSupply.FStackedValues.Count>0 then
        //begin
          ViewDialog.sgrdDemandChannels.Cells[ViewDialog.sgrdDemandChannels.ColCount-1,ARow] := FormatFloat('0.0000',LTotal);
          ViewDialog.sgrdChannelSummary.Alignment := taCenter;
          ViewDialog.sgrdChannelSummary.Cells[0,ARow] := IntToStr(AChannelNumber);
          ViewDialog.sgrdChannelSummary.Cells[1,ARow] := TYieldModelDataObject(FAppModules.Model.ModelData).
                                                         NetworkElementData.ChannelList.
                                                         ChannelByChannelNumber[AChannelNumber].ChannelName;
          ViewDialog.sgrdChannelSummary.Cells[2,ARow] := FormatFloat('0.0000',LLongtermSupply.ActualDemand);
          ViewDialog.sgrdChannelSummary.Cells[3,ARow] := IntToStr(LLongtermSupply.NoOfSeqFullDemObtained);
          ViewDialog.sgrdChannelSummary.Cells[4,ARow] := IntToStr(LLongtermSupply.NoOfSeqDemNotObtained);
          ViewDialog.sgrdChannelSummary.Cells[5,ARow] := FormatFloat('0.0000',LLongtermSupply.BreakPointRI);

        if LLongtermSupply.FStackedValues.Count>0 then
        begin
          if not LLongtermSupply.CriteriaStatus then
            ViewDialog.sgrdChannelSummary.Cells[6,ARow] := 'No'
          else
            ViewDialog.sgrdChannelSummary.Cells[6,ARow] := 'Yes';
        end;

      end
      else
      begin
        ShowMessage('This channel is not in the output');
        if ViewDialog.lsvDemandChannels.ItemIndex >= 0 then
          ViewDialog.lsvDemandChannels.Checked[ViewDialog.lsvDemandChannels.ItemIndex] := False;
        ViewDialog.sgrdDemandChannels.RowCount := ViewDialog.sgrdDemandChannels.RowCount-1;
        ViewDialog.sgrdChannelSummary.RowCount := ViewDialog.sgrdChannelSummary.RowCount-1;
      end;
      LSelectedValues := TStringList.Create;
      try
        for LIndex := 0 to ViewDialog.lsvDemandChannels.Count-1 do
        begin
          if ViewDialog.lsvDemandChannels.Checked[LIndex] then
            LSelectedValues.Add(IntToStr(integer(ViewDialog.lsvDemandChannels.Items.Objects[LIndex])));
        end;

        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','Model',FAppModules.StudyArea.ModelCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','StudyAreaName',FAppModules.StudyArea.StudyAreaCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','SubArea',FAppModules.StudyArea.SubAreaCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','Scenario',FAppModules.StudyArea.ScenarioCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','SelectedValues',LSelectedValues.CommaText);
      finally
        LSelectedValues.Free;
      end;
    end;
    Result := true;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


procedure TOutputDemandChannelsGridSummaryValidator.DoClickChannels(Sender: TObject);
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.DoClickChannels';
var
  LChannelNumber : integer;
  LOldCursor      : TCursor;
begin
  try
    LOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    LChannelNumber := integer(ViewDialog.lsvDemandChannels.Items.Objects[ViewDialog.lsvDemandChannels.ItemIndex]);
    if ViewDialog.lsvDemandChannels.Checked[ViewDialog.lsvDemandChannels.ItemIndex] then
    begin
      ViewDialog.sgrdDemandChannels.RowCount := ViewDialog.sgrdDemandChannels.RowCount+1;
      ViewDialog.sgrdChannelSummary.RowCount := ViewDialog.sgrdChannelSummary.RowCount+1;
      PopulateTableData(LChannelNumber,ViewDialog.sgrdDemandChannels.RowCount-1);
    end
    else
      DeleteTableData(LChannelNumber);

    Screen.Cursor := LOldCursor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.DeleteTableData(AChannelNumber : integer);
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.DeleteTableData';
var
  LIndex : integer;
  LSelectedValues : TStringList;
  LModel,
  LStudyAreaName,
  LSubArea,
  LScenario,
  LSelectedData : string;
begin
  try
    LSelectedValues := TStringList.Create;
    try
      LModel := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','Model','');
      LStudyAreaName := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','StudyAreaName','');
      LSubArea := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','SubArea','');
      LScenario := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','Scenario','');
      if (FAppModules.StudyArea.ModelCode = LModel) and (FAppModules.StudyArea.StudyAreaCode = LStudyAreaName) and
         (FAppModules.StudyArea.SubAreaCode= LSubArea) and
         (FAppModules.StudyArea.ScenarioCode = LScenario) then
        LSelectedData := FAppModules.ViewIni.ReadString('TOutputDataLoadAgent','SelectedValues','');
      LSelectedValues.CommaText := LSelectedData;
      LIndex := LSelectedValues.IndexOf
               (IntToStr(integer(ViewDialog.lsvDemandChannels.Items.
                      Objects[ViewDialog.lsvDemandChannels.ItemIndex])));
      if LIndex >-1 then
      begin
        LSelectedValues.Delete(LIndex);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','Model',FAppModules.StudyArea.ModelCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','StudyAreaName',FAppModules.StudyArea.StudyAreaCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','SubArea',FAppModules.StudyArea.SubAreaCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','Scenario',FAppModules.StudyArea.ScenarioCode);
        FAppModules.ViewIni.WriteString('TOutputDataLoadAgent','SelectedValues',LSelectedValues.CommaText);
        RePopulateDataViewer;
      end;
    finally
      LSelectedValues.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.ClearGrid;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.ClearGrid';
var
  LCol : integer;
  LRow : integer;
begin
  try
    for LCol := 0 to ViewDialog.sgrdDemandChannels.ColCount-1 do
      for LRow := 1 to ViewDialog.sgrdDemandChannels.RowCount-1 do
        ViewDialog.sgrdDemandChannels.Cells[LCol, LRow] := '';
    for LCol := 0 to ViewDialog.sgrdChannelSummary.ColCount-1 do
      for LRow := 1 to ViewDialog.sgrdChannelSummary.RowCount-1 do
        ViewDialog.sgrdChannelSummary.Cells[LCol, LRow] := '';
    ViewDialog.sgrdDemandChannels.RowCount := 2;
    ViewDialog.sgrdChannelSummary.RowCount := 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.DoExport(AFileName: string = '');
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.DoExport';
begin
  try
    ViewDialog.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.DoPrint;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.DoPrint';
begin
  try
    ViewDialog.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryValidator.CanExport: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.CanExport';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputDemandChannelsGridSummaryValidator.CanPrint: boolean;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.CanPrint';
begin
  Result := False;
  try
    if (ViewDialog <> nil) then
      Result := ViewDialog.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.GetSelectionData;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.GetSelectionData';
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
     FUnits        := ouPerSecond;
     FValueType    := LDataSelection.ValueType;
     FTimeStep     := LDataSelection.TimeStep;
     FHighLight    := LDataSelection.Highlight;
     FDisplayMonth := LDataSelection.DisplayMonth;
   end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputDemandChannelsGridSummaryValidator.ChangeViewDataLabel;
const OPNAME = 'TOutputDemandChannelsGridSummaryValidator.ChangeViewDataLabel';
begin
  try
  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

