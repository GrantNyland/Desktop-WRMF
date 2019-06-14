
//
//
//  UNIT      : Contains     Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 24/11/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit URainfallMultiGaugeComparitorValidator;

interface
uses
  SysUtils,
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  VCLTee.Chart,
  VCLTee.Series,
  VCL.Graphics,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  Contnrs,

  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UDataComponent,
  URainfallMultiGaugeComparitorDialog,
  UGenericModelLinkClasses;

type

  TRainfallMultiGaugeComparitorValidator = class (TAbstractDataDialogValidator)
  protected
    FPopulateFlag          : Boolean;
    FCurrentStationID      : integer;
    FCurrentSplitIndex     : integer;
    FCurrentPatchID        : integer;
    FGraphStationID        : integer;
    FGraphSplitIndex       : integer;
    FGraphPatchID          : integer;
    FShowTree              : boolean;
    FNode                  : TTreeNode;
    FNodeList              : TStringList;
    FYearMonthList         : TStringList;
    FNodeData              : TStringList;
    FRowCount,
    FStartYear,
    FEndYear               : integer;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function RainfallMultiGaugeComparitorDialog : TRainfallMultiGaugeComparitorDialog;
    procedure DoGaugeTreeViewMouseDown (Sender : TObject;Button : TMouseButton;Shift  : TShiftState;X, Y   : Integer);
    procedure ClearGrid;
    procedure PopulateGrid;
    function GetRowByYearMonth(AYearIndex,AMonth : integer) : integer;
    procedure GetGridStartEndYear(var AStartYear : integer; var AEndYear : integer);
    procedure BuildGrid(AStartYear,AEndYear : integer);
    procedure GetIdentifiers(ANodeList : string;ALevel : integer; var AStationID : integer;
                             var ASplitIndex : integer;var APatchID : integer);
    procedure RePopulateDataViewer;
    procedure PopulateTreeView;
    procedure GetChartLegend;
  public
    function Initialise: boolean; override;
    procedure PopulateDataViewer; override;
    procedure ClearDataViewer; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : String;
                                  AOldValue  : String;
                                  ANewValue  : String): Boolean; override;
    function StudyHasChanged: boolean; override;

    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure DoToggleTree(ASender : TObject);
    procedure OnClickCreateFiles(Sender: TObject);
    procedure DoCreateSplit(Sender: TObject);
    procedure DoDeleteSplit(Sender: TObject);
  end;

implementation

uses
  DateUtils,
  VCL.ImgList,
  UDatasetType,
  VCL.Printers,
  UConstants,
  RainfallCom_TLB,
  USplitRainfallRecordForm,
  UErrorHandlingOperations, URainfallCommonGaugeSheet, VCL.Grids;

const
  CHydroMonthsDesc : array[1..12]of string = ('Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep');
 
{ TRainfallMultiGaugeComparitorValidator }

procedure TRainfallMultiGaugeComparitorValidator.CreateMemberObjects;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TRainfallMultiGaugeComparitorDialog.Create(nil, FAppModules);
    RainfallMultiGaugeComparitorDialog.GaugeTreeview.OnMouseDown := DoGaugeTreeViewMouseDown;
    FNodeList := TStringList.Create;
    FYearMonthList := TStringList.Create;
    FNodeData := TStringList.Create;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallMultiGaugeComparitorValidator.DestroyMemberObjects;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FNodeList);
    FreeAndNil(FYearMonthList);
    FreeAndNil(FNodeData);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallMultiGaugeComparitorValidator.Initialise: boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FPopulateFlag      := True;
    FShowTree          := True;
    FCurrentStationID  := 0;
    FCurrentSplitIndex := -1;
    FCurrentPatchID    := 0;
    FGraphStationID    := 0;
    FGraphSplitIndex   := -1;
    FGraphPatchID      := 0;
    FStartYear         := 0;
    FEndYear           := 0;
    FYearMonthList.Clear;
    FNodeData.Clear;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallMultiGaugeComparitorValidator.RainfallMultiGaugeComparitorDialog : TRainfallMultiGaugeComparitorDialog;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.RainfallMultiGaugeComparitorDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TRainfallMultiGaugeComparitorDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallMultiGaugeComparitorValidator.StudyHasChanged: boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallMultiGaugeComparitorValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                           AFieldName : string;
                                                           AOldValue  : string;
                                                           ANewValue  : string): Boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallMultiGaugeComparitorValidator.PopulateDataViewer;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.RePopulateDataViewer;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.RePopulateDataViewer';
begin
  try
    with RainfallMultiGaugeComparitorDialog do
    begin
      PanelGrid.Height := Trunc((PanelClient.Height - PanelButton.Height));
    end;
    FCurrentStationID  := (FAppModules.Model.ModelData as IRainfallModelData).CurrentStationID;
    FCurrentSplitIndex := (FAppModules.Model.ModelData as IRainfallModelData).CurrentSplitIndex;
    FCurrentPatchID    := (FAppModules.Model.ModelData as IRainfallModelData).CurrentPatchID;
    PopulateTreeView;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.PopulateTreeView;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.PopulateTreeView';
var
  LIndex         : Integer;
  LPatchIndex    : Integer;
  LCurrentNode   : TTreeNode;
  LStationID     : integer;
  lSelectedNode  : TTreeNode;
  lSplitNode     : TTreeNode;
  lPatchNode     : TTreeNode;
  lPatchID       : integer;
  lRainfallObj   : IRainfallModelData;
  lStation       : IStationData;
  lPatchData     : IPatchData;
  lStationNumber : string;
  lSplitIndex    : integer;
  lSplit         : IRainfallDataSplit;
  lSplitName     : string;
  lSrcPatchID    : integer;
  lTarget        : WideString;
  lStartYear     : integer;
  lEndYear       : integer;
  lFoundSplit    : boolean;
begin
  try
    with RainfallMultiGaugeComparitorDialog do
    begin
      FPopulateFlag := FALSE;
      GaugeTreeview.Items.Clear;
      lSelectedNode := nil;
      lRainfallObj  := (FAppModules.Model.ModelData as IRainfallModelData);
      if (lRainfallObj.StationCount = 0) then
      begin
        GaugeGrid.Visible     := False;
      end
      else
      begin
        for LIndex := 0 to lRainfallObj.StationCount - 1 do
        begin
          lStation := lRainfallObj.GetStationDataByIndex(LIndex);
          if (lStation <> nil) then
          begin
            LStationID := lStation.RainfallData.StationID;
            lStationNumber := lStation.RainfallData.StationNumber;
            if (lStation.IsInWR90) then
              lStationNumber := lStationNumber + ' *';
            LCurrentNode := GaugeTreeview.Items.AddObject(nil, lStationNumber, TObject(LStationID));
            if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID)) then
              lSelectedNode := LCurrentNode;
            for lSplitIndex := 0 to lStation.SplitCount - 1 do
            begin
              lSplit     := lStation.GetSplitWithIndex(lSplitIndex);
              lSplitName := IntToStr(lSplit.HydroStartYear) + ' - ' + IntToStr(lSplit.HydroEndYear);
              lSplitNode := GaugeTreeview.Items.AddChildObject(LCurrentNode, lSplitName, TObject(LStationID));
              if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID) AND
                  (FCurrentSplitIndex <> -1) AND (FCurrentSplitIndex = lSplitIndex)) then
                lSelectedNode := lSplitNode;
            end;
            for LPatchIndex := 0 to lStation.PatchCount - 1 do
            begin
              lPatchData  := lStation.GetPatchWithIndex(LPatchIndex);
              lPatchID    := lPatchData.PatchID;
              lSplitNode  := nil;
              lFoundSplit := FALSE;
              if (lPatchData.PatchTypeID = 1) then
              begin
                lSplitNode  := LCurrentNode.Item[0];
                lFoundSplit := TRUE;
              end
              else
              begin
                lPatchData.GetSourceInfoByStationID(LStationID, lSrcPatchID, lTarget, lStartYear, lEndYear);
                if ((lStartYear = 0) AND (lEndYear = 0)) then
                begin
                  lSplitNode  := LCurrentNode.Item[0];
                  lFoundSplit := TRUE;
                end
                else
                begin
                  lSplitIndex := 0;
                  while ((lSplitIndex < lStation.SplitCount) AND (NOT lFoundSplit)) do
                  begin
                    lSplitNode := LCurrentNode.Item[lSplitIndex];
                    if (lSplitNode.Text = IntToStr(lStartYear) + ' - ' + IntToStr(lEndYear)) then
                      lFoundSplit := TRUE
                    else
                      lSplitIndex := lSplitIndex + 1;
                  end;
                end;
              end;
              if (lFoundSplit) then
              begin
                lPatchNode := GaugeTreeview.Items.AddChildObject(lSplitNode, lPatchData.PatchName, TObject(lPatchID));
                if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID)  AND
                    (FCurrentSplitIndex <> -1) AND (FCurrentSplitIndex = lSplitNode.Index) AND
                    (FCurrentPatchID <> 0) AND (FCurrentPatchID = lPatchID)) then
                  lSelectedNode := lPatchNode;
              end;
            end;
          end;
        end;
        GaugeTreeview.FullExpand;
        FPopulateFlag := TRUE;
        if (lSelectedNode = nil) AND (GaugeTreeview.Items.Count > 0) then
          lSelectedNode := GaugeTreeview.Items[0];
        if (lSelectedNode <> nil) then
        begin
          GaugeTreeview.Selected := lSelectedNode;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.DoGaugeTreeViewMouseDown(Sender : TObject;Button : TMouseButton;Shift  : TShiftState;X, Y   : Integer);
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.DoGaugeTreeViewMouseDown';
var
  LIndex : integer;
  LNode : TTreeNode;
begin
  try
    FNodeList.Clear;
    for LIndex := 0 to RainfallMultiGaugeComparitorDialog.GaugeTreeview.SelectionCount -1 do
    begin
      LNode := RainfallMultiGaugeComparitorDialog.GaugeTreeview.Selections[LIndex];
      if LNode.Level = 0 then
      begin
        if FNodeList.IndexOf(IntToStr(LNode.Level)+'_'+IntToStr(Integer(LNode.Data))+'_0') < 0 then
          FNodeList.Add(IntToStr(LNode.Level)+'_'+IntToStr(Integer(LNode.Data))+'_0');
      end
      else
      if LNode.Level = 1 then
      begin
        if FNodeList.IndexOf(IntToStr(LNode.Level)+'_'+IntToStr(Integer(LNode.Data))+'_'+IntToStr(LNode.Index)) < 0 then
          FNodeList.Add(IntToStr(LNode.Level)+'_'+IntToStr(Integer(LNode.Data))+'_'+IntToStr(LNode.Index));
      end
      else
      if LNode.Level = 2 then
      begin
        if FNodeList.IndexOf(IntToStr(LNode.Level)+'_'+IntToStr(Integer(LNode.Parent.Data))+'_'+IntToStr(Integer(LNode.Data))) < 0 then
          FNodeList.Add(IntToStr(LNode.Level)+'_'+IntToStr(Integer(LNode.Parent.Data))+'_'+IntToStr(Integer(LNode.Data)));
      end;
    end;
    if (FNodeList.Count > 0) then
    begin
      RainfallMultiGaugeComparitorDialog.ClearSeries;
      RainfallMultiGaugeComparitorDialog.SetSeriesCount(FNodeList.Count);
      PopulateGrid;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.ClearGrid;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.ClearGrid';
var
  LIndex    : integer;
  LRowIndex : integer;
begin
  try
    for LRowIndex := 1 to RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.RowCount- 1 do
    begin
      for LIndex := 0 to RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.ColCount - 1 do
      begin
        RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Cells[LIndex, LRowIndex] := '';
        RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Objects[LIndex, LRowIndex] := nil;
      end;
    end;
    RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.RowCount := 2;
    RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.ColCount := 3;
    RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Cells[2,0] := '';
    FYearMonthList.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.PopulateGrid;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.PopulateGrid';
var
  LStartYear : integer;
  LEndYear : integer;
  LMonth : integer;
  LStationIndex : integer;
  LSelectionCount : integer;
  LHydroYearlyData : IYearlyData;
  LStation : IStationData;
  LSplit : IRainfallDataSplit;
  LPatch : IPatchData;
  LLevel,LStationID,
  LSplitIndex,LPatchID : integer;
  LHydroIndex,LYearIndex,
  LRow : integer;
  LValue : double;
  LSeriesValue : double;
  LLineSeries : TLineSeries;
  LBarSeries : TBarSeries;
  LSeriesLabel : string;
  LStartDate : TDateTime;
  LDate   : TDate;
begin
  try

    ClearGrid;
    LStartYear := 0;
    LEndYear := 0;
    LSelectionCount := RainfallMultiGaugeComparitorDialog.GaugeTreeview.SelectionCount;
    RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.ColCount := (LSelectionCount*2) + 2;
    RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.ColWidths[1] := 35;
    RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.RowHeights[0] := 30;
    GetGridStartEndYear(LStartYear,LEndYear);

    for LYearIndex := LStartYear to LEndYear do
    begin
      LHydroYearlyData := nil;
      for LStationIndex := 1 to FNodeList.Count do
      begin

        LLevel := StrToInt(Copy(FNodeList[LStationIndex-1],1,1));
        GetIdentifiers(FNodeList[LStationIndex-1],LLevel,LStationID,LSplitIndex,LPatchID);
        LStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(LStationID);
        LLineSeries := RainfallMultiGaugeComparitorDialog.LineSeriesList[LStationIndex-1];
        LBarSeries := RainfallMultiGaugeComparitorDialog.BarSeriesList[LStationIndex-1];
        if (LStation <> nil) then
        begin
          if LSplitIndex <> -1 then
            LSplit :=  LStation.GetSplitWithIndex(LSplitIndex);
          if (LPatchID = 0) and (LSplit <> nil) then
          begin
            LHydroYearlyData := nil;
            RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Cells[LStationIndex*2,0] :=
            LStation.RainfallData.StationNumber+' '+IntToStr(LSplit.HydroStartYear) + ' - ' + IntToStr(LSplit.HydroEndYear);
            LSeriesLabel :=  LStation.RainfallData.StationNumber+' '+IntToStr(LSplit.HydroStartYear) + ' - ' + IntToStr(LSplit.HydroEndYear);
            for LHydroIndex := 0 to LStation.RainfallData.HydroYearsCount-1 do
            begin
              if (LYearIndex = LStation.RainfallData.GetHydroYearDataByIndex(LHydroIndex).Year) then
              begin
                if (LStation.RainfallData.GetHydroYearDataByIndex(LHydroIndex).Year >= LSplit.HydroStartYear) and
                  (LStation.RainfallData.GetHydroYearDataByIndex(LHydroIndex).Year <= LSplit.HydroEndYear) then
                begin
                  LHydroYearlyData := LStation.RainfallData.GetHydroYearDataByIndex(LHydroIndex);
                  Break;
                end;
              end;
            end;

          end
          else
          if LPatchID > 0 then
          begin
            LPatch := LStation.GetPatchWithID(LPatchID);
            if (LPatch <> nil) then
            begin
              LHydroYearlyData := nil;
              RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Cells[LStationIndex*2,0] :=
              LStation.RainfallData.StationNumber+' '+LPatch.PatchName;
              LSeriesLabel :=  LStation.RainfallData.StationNumber+' '+LPatch.PatchName;
              for LHydroIndex := 0 to LPatch.RainfallData.HydroYearsCount-1 do
              begin
                if ((LYearIndex) = LPatch.RainfallData.GetHydroYearDataByIndex(LHydroIndex).Year) then
                begin
                  LHydroYearlyData := LPatch.RainfallData.GetHydroYearDataByIndex(LHydroIndex);
                  Break;
                end;
              end;

            end;
          end;
        end;

        for LMonth := 1 to 12 do
        begin
          LRow := GetRowByYearMonth(LYearIndex,LMonth);
          if LRow < 0 then
            Continue;
          if LHydroYearlyData <> nil then
          begin
            LValue := LHydroYearlyData.MonthlyRainfall[LMonth];
            if (LValue = NullFloat) then
            begin
              RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Cells[LStationIndex*2,LRow] := '';
              RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.ColWidths[LStationIndex*2] := 60;
              LSeriesValue := 0;
            end
            else
            begin
              RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Cells[LStationIndex*2,LRow] := Format('%6.1f', [LValue]);
              RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.ColWidths[LStationIndex*2] := 60;
              LSeriesValue := LValue;
            end;
            RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Cells[(LStationIndex*2)+1,LRow] := LHydroYearlyData.MonthlyPatchSign[LMonth];
            RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.ColWidths[(LStationIndex*2)+1] := 10;

            if (LLineSeries <> nil) or (LBarSeries <> nil)then
            begin
              LDate  := EncodeDate(LYearIndex, LMonth,1);
              LStartDate := StrToDate(FormatDateTime('yyyy/mm/dd',LDate));
              LLineSeries.AddXY(LStartDate, LSeriesValue);
              LBarSeries.AddXY(LStartDate, LSeriesValue);
              LBarSeries.Visible := False;
              LLineSeries.Title := LSeriesLabel;
              LBarSeries.Title := LSeriesLabel;
            end;
          end;
        end;
      end;
    end;
    if (LEndYear > 0) then
      GetChartLegend;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TRainfallMultiGaugeComparitorValidator.GetChartLegend;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.GetChartLegend';
var
  LIndex : integer;
  LCount : integer;
begin
  try
    RainfallMultiGaugeComparitorDialog.RainfallGraph.Legend.Visible := False;
    LCount := 0;
    for LIndex := 0 to RainfallMultiGaugeComparitorDialog.RainfallGraph.SeriesCount-1 do
    begin
       if (RainfallMultiGaugeComparitorDialog.RainfallGraph.Series[LIndex] is TLineSeries) and
          (RainfallMultiGaugeComparitorDialog.RainfallGraph.Series[LIndex].Title<>'')then
       begin
           RainfallMultiGaugeComparitorDialog.RainfallGraph.Series[LIndex].ShowInLegend := True;
           RainfallMultiGaugeComparitorDialog.RainfallGraph.Series[LIndex].Color        := C_Colors[LCount];
           LCount := LCount+1;
       end
       else
         RainfallMultiGaugeComparitorDialog.RainfallGraph.Series[LIndex].ShowInLegend := False;

       if not (RainfallMultiGaugeComparitorDialog.RainfallGraph.Series[LIndex] is TLineSeries) then
         RainfallMultiGaugeComparitorDialog.RainfallGraph.Series[LIndex].ShowInLegend := False;


    end;

    if (LCount >= 2) then
    begin
      RainfallMultiGaugeComparitorDialog.RainfallGraph.Legend.Visible := True;
      RainfallMultiGaugeComparitorDialog.RainfallGraph.Legend.Alignment := laBottom;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TRainfallMultiGaugeComparitorValidator.GetGridStartEndYear(var AStartYear : integer; var AEndYear : integer);
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.GetGridStartEndYear';
var
  LIndex : integer;
  LLevel : integer;
  LStation : IStationData;
  LSplit : IRainfallDataSplit;
  LPatch : IPatchData;
  LStationID : integer;
  LPatchID : integer;
  LSplitIndex : integer;
begin
  try
    for LIndex := 0 to FNodeList.Count-1 do
    begin
      LLevel := StrToInt(Copy(FNodeList[LIndex],1,1));
      GetIdentifiers(FNodeList[LIndex],LLevel,LStationID,LSplitIndex,LPatchID);
      LStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(LStationID);
      if LStation <> nil then
      begin
        LPatch := LStation.GetPatchWithID(LPatchID);
        if LSplitIndex > -1 then
        LSplit := LStation.GetSplitWithIndex(LSplitIndex);
        if (LPatchID = 0) and (LSplit <> nil) then
        begin
          if (LSplit.HydroStartYear < AStartYear) or (AStartYear = 0) then
            AStartYear := LSplit.HydroStartYear;
          if (LSplit.HydroEndYear > AEndYear) then
            AEndYear := LSplit.HydroEndYear;
        end;
        if LPatchID > 0 then
        begin
          if (LPatch <> nil) then
          begin
            if (LPatch.RainfallData.HydroStartYear < AStartYear) or (AStartYear = 0) then
              AStartYear := LPatch.RainfallData.HydroStartYear;
            if (LPatch.RainfallData.HydroEndYear > AEndYear) or (AEndYear = 0) then
              AEndYear := LPatch.RainfallData.HydroEndYear;
          end;
        end;
      end;
    end;
    BuildGrid(AStartYear,AEndYear);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.BuildGrid(AStartYear,AEndYear : integer);
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.BuildGrid';
var
  LIndex : integer;
  LYear : integer;
  LMonth : integer;
  LRow : integer;
begin
  try
    LRow := 1;
    for LIndex := AStartYear to  AEndYear do
    begin
      RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.RowCount :=
      RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.RowCount + 12;
      for LMonth := 1 to 12 do
      begin
        if LMonth <= 3 then
          LYear := LIndex
        else
          LYear := LIndex + 1;
        RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Cells[0,LRow] := IntToStr(LYear); 
        RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Cells[1,LRow] := CHydroMonthsDesc[LMonth];
        RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.RowHeights[LRow] := 16;
        RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.Color := clSilver;
        FYearMonthList.AddObject(IntToStr(LIndex)+IntToStr(LMonth),TObject(LRow));
        inc(LRow);
      end;
    end;
    RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.RowCount :=
    RainfallMultiGaugeComparitorDialog.MultipleGaugeCompareGrid.RowCount -1;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallMultiGaugeComparitorValidator.GetRowByYearMonth(AYearIndex, AMonth : integer) : integer;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.GetRowByYearMonth';
var
  LIndex : integer;
begin
  Result := -1;
  try
    LIndex := FYearMonthList.IndexOf(IntToStr(AYearIndex)+IntToStr(AMonth));
    if LIndex >= 0 then
      Result := integer(FYearMonthList.Objects[LIndex]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.GetIdentifiers(ANodeList : string;ALevel : integer; var AStationID : integer;
                                                                var ASplitIndex : integer;var APatchID : integer);
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.GetIdentifiers';
var
  LStationStrID : string;
  LPatchStrID : string;
begin
  try
    if ALevel = -1 then
      Exit;
    if (ALevel < 2) then
    begin
      LStationStrID := ANodeList;
      Delete(LStationStrID,1,2);
      AStationID := StrToInt(Copy(LStationStrID,1,Pos('_',LStationStrID)-1));
      Delete(LStationStrID,1,Pos('_',LStationStrID));
      ASplitIndex := StrToInt(LStationStrID);
      APatchID := 0;
    end;
    if (ALevel = 2) then
    begin
      LPatchStrID := ANodeList;
      Delete(LPatchStrID,1,2);
      AStationID := StrToInt(Copy(LPatchStrID,1,Pos('_',LPatchStrID)-1));
      Delete(LPatchStrID,1,Pos('_',LPatchStrID));
      APatchID   := StrToInt(LPatchStrID);
      ASplitIndex := -1;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallMultiGaugeComparitorValidator.CanPrint: Boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.CanPrint';
begin
  Result := True;
  try

  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallMultiGaugeComparitorValidator.CanCopyToClipboard: Boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.CanCopyToClipboard';
begin
  Result := True;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallMultiGaugeComparitorValidator.CanExport: Boolean;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.CanExport';
begin
  Result := True;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.DoPrint;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.DoPrint';
var
  lPrinterOrientation : TPrinterOrientation;
begin
  try
    if (RainfallMultiGaugeComparitorDialog.GaugeTreeview.Selected <> nil) then
    begin
      lPrinterOrientation := Printer.Orientation;
      try
        Printer.Orientation := poLandscape;
        RainfallMultiGaugeComparitorDialog.GaugeGrid.DoPrint(FAppModules.Language.GetString('Rainfall.GaugeStatistics'));
      finally
        Printer.Orientation := lPrinterOrientation;
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemInTreeview'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.DoCopyToClipboard;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.DoCopyToClipboard';
begin
  try
    if (RainfallMultiGaugeComparitorDialog.GaugeTreeview.Selected <> nil) then
      RainfallMultiGaugeComparitorDialog.GaugeGrid.DoCopyToClipboard
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemInTreeview'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.DoExport';
begin
  try
    if(RainfallMultiGaugeComparitorDialog.GaugeTreeview.Selected <> nil) then
    begin
      RainfallMultiGaugeComparitorDialog.DoExport(AFileName);
      RainfallMultiGaugeComparitorDialog.Repaint;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemInTreeview'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.DoCreateSplit (Sender: TObject);
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.DoCreateSplit';
var
  lForm : TSplitRainfallRecordForm;
begin
  try
    lForm := TSplitRainfallRecordForm.CreateWithoutDFM(nil, FAppModules);
    try
      lForm.Initialise;
      lForm.LanguageHasChanged;
      lForm.SetData(FCurrentStationID);
      lForm.ShowModal;
      PopulateTreeView;
    finally
      FreeAndNil(lForm);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.DoDeleteSplit (Sender: TObject);
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.DoDeleteSplit';
var
  lStartYear    : integer;
  lEndYear      : integer;
  lRainfallObj  : IRainfallModelData;
  lStation      : IStationData;
  lSplit        : IRainfallDataSplit;
  lMessage      : string;
begin
  try
    lStation := nil;
    if (FCurrentStationID <> 0) AND (FCurrentSplitIndex <> -1) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lStation     := lRainfallObj.GetStationDataByID(FCurrentStationID);
      if (lStation <> nil) then
      begin
        lSplit     := lStation.GetSplitWithIndex(FCurrentSplitIndex);
        lStartYear := lSplit.HydroStartYear;
        lEndYear   := lSplit.HydroEndYear;
        lSplit     := nil;
        if (lStation.MayDeleteSplit(lStartYear, lEndYear)) then
        begin
          lRainfallObj.DeleteASplit(lStation.RainfallData.StationID, lStartYear, lEndYear);
          PopulateTreeView;
        end
        else
        begin
          lMessage := FAppModules.Language.GetString('Rainfall.MayNotDeleteSplit');
          lMessage := Format(lMessage, [lStation.RainfallData.StationNumber, lStartYear, lEndYear]);
          ShowMessage(lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.DoToggleTree;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.DoToggleTree';
begin
  try
    FShowTree := NOT FShowTree;
    with RainfallMultiGaugeComparitorDialog do
    begin
      PanelTreeView.Visible := FShowTree;
      PanelLeft.Visible     := FShowTree;
      Splitter.Left         := PanelLeft.Left + (PanelLeft.Width + 1);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.OnClickCreateFiles(Sender: TObject);
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.OnClickCreateFiles';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallMultiGaugeComparitorValidator.ClearDataViewer;
const OPNAME = 'TRainfallMultiGaugeComparitorValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearGrid;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
