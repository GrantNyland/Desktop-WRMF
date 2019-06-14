{******************************************************************************}
{*  UNIT      : Contains TRainfallPatchAdminValidator Class                   *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 03/05/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallPatchAdminValidator;

interface

uses
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  Contnrs,
  VCLTee.GanttCh,
  VCLTee.TeEngine,

  VCL.Graphics,
  VCL.Grids,

  UDataComponent,
  URainfallPatchAdminDialog,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  URainfallCommonGaugeSheet,
  UMenuItemManager,
  URainfallPatchAdminMenuItemManager,
  UGenericModelLinkClasses;

type

  TRainfallPatchAdminValidator = class (TAbstractDataDialogValidator)
  protected
    FPopulateFlag      : Boolean;
    FCurrentPatchID    : integer;
    FCurrentStationID  : integer;
    FCurrentSplitIndex : integer;
    FAddGaugeNode      : TTreeNode;
    FGraphRatio        : double;
    FSystemFlag        : boolean;
    FShowGrid          : boolean;
    FShowGraph         : boolean;
    FShowTree          : boolean;
    FPatchRMaxGauges   : integer;
    FMenuItemManager   : TRainfallPatchAdminMenuItemManager;
    FNode              : TTreeNode;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function RainfallPatchAdminDialog : TRainfallPatchAdminDialog;

    procedure DoGaugeTreeViewChanged(ASender: TObject; ANode: TTreeNode);
    procedure ResetButtons;
    procedure DoIncludeUnreliableCheckBoxClick(Sender : TObject);
    procedure ClearGrid;
    procedure PopulateGrid;
    procedure PopulateGraph;
    procedure PopulateTreeView;

    function CreateANewPatch (APatchDescription : string;
                              AStationID        : integer;
                              ASplitIndex       : integer) : integer;
    function UpdatePatch(AStationID      : integer;
                         APatchID        : integer;
                         ANewDescription : string) : boolean;
    function DeletePatch (APatchID : integer) : boolean;
    function AddToPatch (ASourceStationID  : integer;
                         ASourcePatchID    : integer;
                         AStartYear        : integer;
                         AEndYear          : integer): Boolean;
    function DeleteFromPatch (APatchID         : integer;
                              ASourceStationID : integer;
                              ASourcePatchID   : integer): Boolean;

    procedure DoPlotSplitterMoved (Sender : TObject);
    procedure DoDrawGrid (Sender : TObject;
                          ACol   : integer;
                          ARow   : Integer;
                          Rect   : TRect;
                          State  : TGridDrawState);
    procedure DoGetAxisLabel (Sender         : TChartAxis;
                              ASeries        : TChartSeries;
                              AValueIndex    : integer;
                              var ALabelText : String );
    function AddSeriesToChart (ASeries      : TGanttSeries;
                               AStartDate   : TDateTime;
                               AEndDate     : TDateTime;
                               AIndex       : integer;
                               ADescription : string;
                               AColor       : TColor ) : boolean;
    procedure AddSeriesWithGapsToChart (AIndex       : integer;
                                        AStationID   : integer;
                                        AStationName : string;
                                        AColor       : TColor;
                                        AStartYear   : integer;
                                        AEndYear     : integer;
                                        ADullColor   : TColor);
    procedure AddSplitSeriesToChart (AStartDate      : TDateTime;
                                     AEndDate        : TDateTime;
                                     AIndex          : integer;
                                     AStationName    : string;
                                     AColor          : TColor;
                                     ASplitStartDate : TDateTime;
                                     ASplitEndDate   : TDateTime;
                                     ADullColor      : TColor);
    procedure RePopulateDataViewer;
  public
    procedure DoCreatePatch(ASender : TObject);
    procedure DoUpdatePatch(ASender : TObject);
    procedure DoDeletePatch(ASender : TObject);
    procedure DoSetAddGaugeMode(ASender : TObject);
    procedure DoAddGauge(ASender : TObject);
    procedure DoRemoveGauge(ASender : TObject);
    procedure DoToggleGrid(ASender : TObject);
    procedure DoToggleGraph(ASender : TObject);
    procedure DoToggleTree(ASender : TObject);
    procedure DisplayGridGraph;

    function Initialise: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    function StudyHasChanged: boolean; override;
    procedure PopulateDataViewer; override;
    procedure SetPopupRenameClick(Sender : TObject);
    procedure SetPopupRemoveClick(Sender : TObject);

    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function SaveState: Boolean; override;
    property MenuItemManager   : TRainfallPatchAdminMenuItemManager read FMenuItemManager write FMenuItemManager;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.ImgList,
  VCL.Printers,

  UConstants,
  UDatasetType,
  RainfallCom_TLB,
  UErrorHandlingOperations,
  DateUtils;

{ TRainfallPatchAdminValidator }

procedure TRainfallPatchAdminValidator.CreateMemberObjects;
const OPNAME = 'TRainfallPatchAdminValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TRainfallPatchAdminDialog.Create(nil, FAppModules);

    with RainfallPatchAdminDialog do
    begin
      GaugeGrid.OnDrawCell             := DoDrawGrid;
      HorSplitter.OnMoved              := DoPlotSplitterMoved;
      RecordLengthChart.OnGetAxisLabel := DoGetAxisLabel;
      GaugeTreeview.OnChange           := DoGaugeTreeViewChanged;
      IncludeUnreliableChkBox.OnClick  := DoIncludeUnreliableCheckBoxClick;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallPatchAdminValidator.DestroyMemberObjects;
const OPNAME = 'TRainfallPatchAdminValidator.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallPatchAdminValidator.Initialise: boolean;
const OPNAME = 'TRainfallPatchAdminValidator.Initialise';
var
  lMaxGauges : string;
begin
  Result := inherited Initialise;
  try
    FGraphRatio        := 0.6;
    FSystemFlag        := FALSE;
    FPopulateFlag      := TRUE;
    FShowGrid          := TRUE;
    FShowGraph         := TRUE;
    FShowTree          := TRUE;
    FAddGaugeNode      := nil;
    FCurrentStationID  := 0;
    FCurrentSplitIndex := -1;
    FCurrentPatchID    := 0;
    lMaxGauges         := FAppModules.ViewIni.ReadString('TPatchRValidator', 'PatchRGaugesMax', 'DefaultMaxGauges');
    FPatchRMaxGauges   := StrToInt(lMaxGauges);

    if (FMenuItemManager <> nil) then
    begin
      FMenuItemManager.SetCreatePatch(FALSE);
      FMenuItemManager.SetDeletePatch(FALSE);
      FMenuItemManager.SetRenamePatch(FALSE);
      FMenuItemManager.SetAddGaugeToPatch(FALSE);
      FMenuItemManager.SetRemoveGaugeFromPatch(FALSE);
      FMenuItemManager.SetToggleGrid(TRUE);
      FMenuItemManager.SetToggleGraph(TRUE);
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallPatchAdminValidator.RainfallPatchAdminDialog : TRainfallPatchAdminDialog;
const OPNAME = 'TRainfallPatchAdminValidator.RainfallPatchAdminDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TRainfallPatchAdminDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallPatchAdminValidator.StudyHasChanged: boolean;
const OPNAME = 'TRainfallPatchAdminValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallPatchAdminValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                           AFieldName : string;
                                                           AOldValue  : string;
                                                           ANewValue  : string): Boolean;
const OPNAME = 'TRainfallPatchAdminValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallPatchAdminValidator.PopulateDataViewer;
const OPNAME = 'TRainfallPatchAdminValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    with RainfallPatchAdminDialog do
    begin
      PanelGrid.Height := Trunc((PanelClient.Height - PanelButton.Height - HorSplitter.Height) *
                                (1 - FGraphRatio));
    end;
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.RePopulateDataViewer;
const OPNAME = 'TRainfallPatchAdminValidator.RePopulateDataViewer';
begin
  try
    FAddGaugeNode := nil;

    if (FCurrentStationID = 0) AND (FCurrentSplitIndex = -1) AND (FCurrentPatchID = 0) then
    begin
      FCurrentStationID  := (FAppModules.Model.ModelData as IRainfallModelData).PatchRStationID;
      FCurrentSplitIndex := (FAppModules.Model.ModelData as IRainfallModelData).PatchRSplitIndex;
      FCurrentPatchID    := (FAppModules.Model.ModelData as IRainfallModelData).PatchRPatchID;
    end;
    RainfallPatchAdminDialog.IncludeUnreliableChkBox.Checked :=
      (FAppModules.Model.ModelData as IRainfallModelData).IncludeUnreliableData;
    PopulateTreeView;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.PopulateTreeView;
const OPNAME = 'TRainfallPatchAdminValidator.PopulateTreeView';
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
  lOldFlag       : Boolean;
begin
  try
    with RainfallPatchAdminDialog do
    begin
      lOldFlag      := FPopulateFlag;
      FPopulateFlag := FALSE;
      GaugeTreeview.Items.Clear;
      FPopulateFlag := lOldFlag;
      lSelectedNode := nil;
      lRainfallObj  := (FAppModules.Model.ModelData as IRainfallModelData);
      for LIndex := 0 to lRainfallObj.StationCount - 1 do
      begin
        lStation := lRainfallObj.GetStationDataByIndex(LIndex);
        if (lStation <> nil) then
        begin
          lStationID := lStation.RainfallData.StationID;
          lStationNumber := lStation.RainfallData.StationNumber;
          if (lStation.IsInWR90) then
            lStationNumber := lStationNumber + ' *';
          LCurrentNode := GaugeTreeview.Items.AddObject(nil, lStationNumber, TObject(LStationID));
          if ((FCurrentStationID <> 0) AND (FCurrentStationID = lStationID)) then
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
              if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID)) AND
                  ((FCurrentSplitIndex <> -1 ) AND (FCurrentSplitIndex = lSplitNode.Index)) AND
                  ((FCurrentPatchID <> 0) AND (FCurrentPatchID = lPatchID)) then
                lSelectedNode := lPatchNode;
            end;
          end;
        end;
      end;
      GaugeTreeview.FullExpand;
      if (lSelectedNode <> nil) then
        GaugeTreeview.Selected := lSelectedNode
      else
      begin
        PopulateGrid;
        PopulateGraph;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.ResetButtons;
const OPNAME = 'TRainfallPatchAdminValidator.ResetButtons';
var
  lPatch             : IPatchData;
  lStation           : IStationData;
  lRainfallModelData : IRainfallModelData;
begin
  try
    if (FMenuItemManager <> nil) then
    begin
      FMenuItemManager.SetCreatePatch(FALSE);
      FMenuItemManager.SetDeletePatch(FALSE);
      FMenuItemManager.SetRenamePatch(FALSE);
      FMenuItemManager.SetAddGaugeToPatch(FALSE);
      FMenuItemManager.SetRemoveGaugeFromPatch(FALSE);
      FMenuItemManager.SetToggleGrid(FALSE);
      FMenuItemManager.SetToggleGraph(FALSE);
      if (FAddGaugeNode = nil) then
      begin
        FMenuItemManager.SetToggleGrid(TRUE);
        FMenuItemManager.SetToggleGraph(TRUE);
        with RainfallPatchAdminDialog do
        begin
          if (GaugeTreeview.Selected <> nil) then
          begin
            if (GaugeTreeview.Selected.Level = 1) then
              FMenuItemManager.SetCreatePatch(TRUE)
            else
            if (GaugeTreeview.Selected.Level = 2) then
            begin
              lRainfallModelData := (FAppModules.Model.ModelData as IRainfallModelData);
              lStation           := lRainfallModelData.GetStationDataByID(FCurrentStationID);
              lPatch             := nil;
              if (lStation <> nil) then
                lPatch := lStation.GetPatchWithID(FCurrentPatchID);
              if (lPatch <> nil) AND (lPatch.PatchTypeID <> 1) then
              begin
                FMenuItemManager.SetDeletePatch(TRUE);
                FMenuItemManager.SetRenamePatch(TRUE);
                FMenuItemManager.SetRemoveGaugeFromPatch(TRUE);
                if (lPatch.SourcesCount < FPatchRMaxGauges) then
                  FMenuItemManager.SetAddGaugeToPatch(TRUE);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoGaugeTreeViewChanged (ASender : TObject;
                                                               ANode   : TTreeNode);
const OPNAME = 'TRainfallPatchAdminValidator.DoGaugeTreeViewChanged';
begin
  try
    if (FPopulateFlag) AND (Assigned(FAppModules.Model())) AND (Assigned(FAppModules.Model.ModelData())) then
    begin
      with RainfallPatchAdminDialog do
      begin
        PanelGrid.Height := Trunc((PanelClient.Height - PanelButton.Height - HorSplitter.Height) *
                                   (1 - FGraphRatio));
        if (GaugeTreeview.Selected <> nil) then
        begin
          if (FAddGaugeNode <> nil) then
          begin
            FAddGaugeNode := nil;
            DoAddGauge(Self);
          end
          else
          begin
            FNode := ANode;
            if (GaugeTreeview.Selected.Level = 0) then
            begin
                FCurrentStationID   := Integer(GaugeTreeview.Selected.Data);
                FCurrentSplitIndex  := -1;
                FCurrentPatchID     := 0;
            end
            else
            if (GaugeTreeview.Selected.Level = 1) then
            begin
                FCurrentStationID  := Integer(GaugeTreeview.Selected.Data);
                FCurrentSplitIndex := GaugeTreeview.Selected.Index;
                FCurrentPatchID    := 0;
            end
            else
            if (GaugeTreeview.Selected.Level = 2) then
            begin
              FCurrentStationID  := Integer(GaugeTreeview.Selected.Parent.Data);
              FCurrentSplitIndex := GaugeTreeview.Selected.Parent.Index;
              FCurrentPatchID    := Integer(GaugeTreeview.Selected.Data);
            end;
            (FAppModules.Model.ModelData as IRainfallModelData).PatchRStationID  := FCurrentStationID;
            (FAppModules.Model.ModelData as IRainfallModelData).PatchRSplitIndex := FCurrentSplitIndex;
            (FAppModules.Model.ModelData as IRainfallModelData).PatchRPatchID    := FCurrentPatchID;

            ResetButtons;
            PopulateGrid;
            PopulateGraph;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.ClearGrid;
const OPNAME = 'TRainfallPatchAdminValidator.ClearGrid';
var
  LIndex    : integer;
  LRowIndex : integer;
begin
  try
    with RainfallPatchAdminDialog do
    begin
      for LRowIndex := 1 to GaugeGrid.RowCount- 1 do
        for LIndex := 0 to GaugeGrid.ColCount - 1 do
          GaugeGrid.Cells[LIndex, LRowIndex] := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.PopulateGrid;
const OPNAME = 'TRainfallPatchAdminValidator.PopulateGrid';
var
  lIndex           : integer;
  lRow             : integer;
  lColIndex        : integer;
  LStationID       : integer;
  lRainfallData    : IRainfallModelData;
  lStation         : IStationData;
  lSplit           : IRainfallDataSplit;
  lPatch           : IPatchData;
  lValue           : double;
  lSourceStationID : integer;
  lSourcePatchID   : integer;
  lTargetStation   : WideString;
  lSourceStation   : IStationData;
  lSourcePatch     : IPatchData;
  lSourceSplit     : IRainfallDataSplit;
  lLatStr          : WideString;
  lLonStr          : WideString;
  lStartYear       : integer;
  lEndYear         : integer;
  lRawString       : string;
  lNotAppString    : string;
begin
  try
    lRawString := FAppModules.Language.GetString('Rainfall.RAW');
    lNotAppString := FAppModules.Language.GetString('Rainfall.NotApplicable');
    with RainfallPatchAdminDialog do
    begin
      PatchMsgLbl.Caption := '';
      if FPopulateFlag then
      begin
        if (GaugeTreeview.Selected = nil) then
        begin
          GaugeGrid.RowCount := 2;
          ClearGrid;
        end
        else
        begin
          lRainfallData := (FAppModules.Model.ModelData as IRainfallModelData);
          ClearGrid;
          if (FCurrentPatchID <> 0) then
          begin
            GaugeGrid.RowCount := 6;
            GaugeGrid.RowHeights[2] := 8;
            GaugeGrid.RowHeights[4] := 8;
            LStationID := Integer(GaugeTreeview.Selected.Parent.Data);
          end
          else
          begin
            GaugeGrid.RowCount := 2;
            LStationID := Integer(GaugeTreeview.Selected.Data);
          end;

          lStation := lRainFallData.GetStationDataByID(LStationID);
          if FCurrentSplitIndex = -1 then
            FCurrentSplitIndex := 0;
          if (lStation <> nil) then
            lSplit := lStation.GetSplitWithIndex(FCurrentSplitIndex);
          if (lSplit <> nil) then
          begin
            lStation.LatLong(lLatStr, lLonStr);
            GaugeGrid.Cells[0, 1]  := lStation.RainfallData.StationNumber;
            GaugeGrid.Cells[1, 1]  := lStation.StationName;
            GaugeGrid.Cells[2, 1]  := lRawString;
            GaugeGrid.Cells[3, 1]  := lLatStr;
            GaugeGrid.Cells[4, 1]  := lLonStr;
            GaugeGrid.Cells[5, 1]  := IntToStr(lSplit.HydroStartYear);
            GaugeGrid.Cells[6, 1]  := IntToStr(lSplit.HydroEndYear);
            if (RainfallPatchAdminDialog.IncludeUnreliableChkBox.Checked) then
            begin
              GaugeGrid.Cells[7, 1]  := Format('%6.1f', [lSplit.MAP]);
              GaugeGrid.Cells[8, 1]  := Format('%6.1f', [lSplit.StdDeviation]);
              GaugeGrid.Cells[9, 1]  := Format('%4.1f', [lSplit.CV]);
            end
            else
            begin
              GaugeGrid.Cells[7, 1]  := Format('%6.1f', [lSplit.XMAP]);
              GaugeGrid.Cells[8, 1]  := Format('%6.1f', [lSplit.XStdDeviation]);
              GaugeGrid.Cells[9, 1]  := Format('%4.1f', [lSplit.XCV]);
            end;
            if ((lSplit.HydroEndYear = 0) AND (lSplit.HydroStartYear = 0)) then
              lValue := 0
            else
              lValue := (lSplit.HydroEndYear - lSplit.HydroStartYear + 1);
            GaugeGrid.Cells[11, 1] := Format('%4d', [Trunc(lValue)]);
            if (lValue <> 0) then
              lValue := lSplit.NrOfMissingMonths * 100 / (lValue * 12);
            GaugeGrid.Cells[10, 1] := Format('%6.2f', [lValue]);
            if (lStation.Height <> NullInteger) then
              GaugeGrid.Cells[12, 1] := Format('%4d', [lStation.Height])
            else
              GaugeGrid.Cells[12, 1] := '';
            GaugeGrid.Cells[13, 1] := lStation.StationType;
          end
          else
          begin
            for lColIndex := 5 to 11 do
              GaugeGrid.Cells[lColIndex, 1] := lNotAppString;
          end;
          if ((FCurrentPatchID <> 0) AND (lSplit <> nil)) then
          begin
            lPatch := lStation.GetPatchWithID(FCurrentPatchID);
            if (lPatch <> nil) then
            begin
              GaugeGrid.RowCount := 5 + lPatch.SourcesCount - 1;
              GaugeGrid.Cells[2, 3]  := lPatch.PatchName;
              if ((lPatch.PatchTypeID = 1) OR (lPatch.ChangeDate < lPatch.PatchROutputDate)) then
              begin
                GaugeGrid.Cells[5, 3]  := IntToStr(lPatch.RainfallData.StartYear);
                GaugeGrid.Cells[6, 3]  := IntToStr(lPatch.RainfallData.EndYear);
                if (RainfallPatchAdminDialog.IncludeUnreliableChkBox.Checked) then
                begin
                  GaugeGrid.Cells[7, 3]  := Format('%6.1f', [lPatch.RainfallData.MAP]);
                  GaugeGrid.Cells[8, 3]  := Format('%6.1f', [lPatch.RainfallData.StdDeviation]);
                  GaugeGrid.Cells[9, 3]  := Format('%4.1f', [lPatch.RainfallData.CV]);
                end
                else
                begin
                  GaugeGrid.Cells[7, 3]  := Format('%6.1f', [lPatch.RainfallData.XMAP]);
                  GaugeGrid.Cells[8, 3]  := Format('%6.1f', [lPatch.RainfallData.XStdDeviation]);
                  GaugeGrid.Cells[9, 3]  := Format('%4.1f', [lPatch.RainfallData.XCV]);
                end;
                if ((lPatch.RainfallData.EndYear = 0) AND (lPatch.RainfallData.StartYear = 0)) then
                  lValue := 0
                else
                  lValue := (lPatch.RainfallData.EndYear - lPatch.RainfallData.StartYear + 1);
                GaugeGrid.Cells[11, 3] := Format('%4d', [Trunc(lValue)]);
                if (lValue <> 0) then
                  lValue := lPatch.RainfallData.NrOfMissingMonths * 100 / (lValue * 12);
                GaugeGrid.Cells[10, 3] := Format('%6.2f', [lValue]);
              end
              else
              begin
                if (lPatch.PatchTypeID <> 1) then
                begin
                  if (lPatch.PatchROutputDate = 0) then
                    PatchMsgLbl.Caption := FAppModules.Language.GetString('LabelText.PatchNotRun')
                  else
                  if (lPatch.ChangeDate > lPatch.PatchROutputDate) then
                    PatchMsgLbl.Caption := FAppModules.Language.GetString('LabelText.ResultsNotValid');
                end;
                for lColIndex := 5 to 11 do
                  GaugeGrid.Cells[lColIndex, 3] := lNotAppString;
              end;
            end
            else
            begin
              for lColIndex := 5 to 11 do
                GaugeGrid.Cells[lColIndex, 3] := lNotAppString;
            end;
            lRow := -1;
            if lPatch <> nil then
            begin
              for lIndex := 0 to lPatch.SourcesCount - 1 do
              begin
                lSourcePatch := nil;
                lPatch.GetSourceInfoByIndex(lIndex, lSourceStationID, lSourcePatchID,
                                            lTargetStation, lStartYear, lEndYear);
                if ((lSourceStationID <> 0) AND (lSourceStationID <> LStationID)) then
                begin
                  lRow := lRow + 1;
                  lSourceStation := lRainfallData.GetStationDataByID(lSourceStationID);
                  lSourceSplit   := lSourceStation.GetSplitForYears(lStartYear, lEndYear);
                  if (lSourcePatchID <> 0) then
                    lSourcePatch := lSourceStation.GetPatchWithID(lSourcePatchID);

                  GaugeGrid.Objects[0 , lRow+5] := TObject(lSourceStationID);
                  GaugeGrid.Objects[1 , lRow+5] := TObject(lSourcePatchID);
                  if (lSourcePatch <> nil) then
                  begin
                    lSourceStation.LatLong(lLatStr, lLonStr);
                    GaugeGrid.Cells[0, lRow+5]  := lSourceStation.RainfallData.StationNumber;
                    GaugeGrid.Cells[1, lRow+5]  := lSourceStation.StationName;
                    GaugeGrid.Cells[2, lRow+5]  := lSourcePatch.PatchName;
                    GaugeGrid.Cells[3, lRow+5]  := lLatStr;
                    GaugeGrid.Cells[4, lRow+5]  := lLonStr;
                    GaugeGrid.Cells[5, lRow+5]  := IntToStr(lSourcePatch.RainfallData.StartYear);
                    GaugeGrid.Cells[6, lRow+5]  := IntToStr(lSourcePatch.RainfallData.EndYear);
                    if (RainfallPatchAdminDialog.IncludeUnreliableChkBox.Checked) then
                    begin
                      GaugeGrid.Cells[7, lRow+5]  := Format('%6.1f', [lSourcePatch.RainfallData.MAP]);
                      GaugeGrid.Cells[8, lRow+5]  := Format('%6.1f', [lSourcePatch.RainfallData.StdDeviation]);
                      GaugeGrid.Cells[9, lRow+5]  := Format('%4.1f', [lSourcePatch.RainfallData.CV]);
                    end
                    else
                    begin
                      GaugeGrid.Cells[7, lRow+5]  := Format('%6.1f', [lSourcePatch.RainfallData.XMAP]);
                      GaugeGrid.Cells[8, lRow+5]  := Format('%6.1f', [lSourcePatch.RainfallData.XStdDeviation]);
                      GaugeGrid.Cells[9, lRow+5]  := Format('%4.1f', [lSourcePatch.RainfallData.XCV]);
                    end;
                    if ((lSourcePatch.RainfallData.EndYear = 0) AND (lSourcePatch.RainfallData.StartYear = 0)) then
                      lValue := 0
                    else
                      lValue := (lSourcePatch.RainfallData.EndYear - lSourcePatch.RainfallData.StartYear + 1);
                    GaugeGrid.Cells[11, lRow+5] := Format('%4d', [Trunc(lValue)]);
                    if (lValue <> 0) then
                      lValue := lSourcePatch.RainfallData.NrOfMissingMonths * 100 / (lValue * 12);
                    GaugeGrid.Cells[10, lRow+5] := Format('%6.2f', [lValue]);
                    if (lSourceStation.Height <> NullInteger) then
                      GaugeGrid.Cells[12, lRow+5] := Format('%4d', [lSourceStation.Height])
                    else
                      GaugeGrid.Cells[12, lRow+5] := '';
                    GaugeGrid.Cells[13, lRow+5] := lSourceStation.StationType;
                  end
                  else
                  if (lSourceSplit <> nil) then
                  begin
                    lSourceStation.LatLong(lLatStr, lLonStr);
                    GaugeGrid.Objects[0 , lRow+5] := TObject(lSourceStationID);
                    GaugeGrid.Cells[0, lRow+5]  := lSourceStation.RainfallData.StationNumber;
                    GaugeGrid.Cells[1, lRow+5]  := lSourceStation.StationName;
                    GaugeGrid.Cells[2, lRow+5]  := lRawString;
                    GaugeGrid.Cells[3, lRow+5]  := lLatStr;
                    GaugeGrid.Cells[4, lRow+5]  := lLonStr;
                    GaugeGrid.Cells[5, lRow+5]  := IntToStr(lSourceSplit.HydroStartYear);
                    GaugeGrid.Cells[6, lRow+5]  := IntToStr(lSourceSplit.HydroEndYear);
                    if (RainfallPatchAdminDialog.IncludeUnreliableChkBox.Checked) then
                    begin
                      GaugeGrid.Cells[7, lRow+5]  := Format('%6.1f', [lSourceSplit.MAP]);
                      GaugeGrid.Cells[8, lRow+5]  := Format('%6.1f', [lSourceSplit.StdDeviation]);
                      GaugeGrid.Cells[9, lRow+5]  := Format('%4.1f', [lSourceSplit.CV]);
                    end
                    else
                    begin
                      GaugeGrid.Cells[7, lRow+5]  := Format('%6.1f', [lSourceSplit.XMAP]);
                      GaugeGrid.Cells[8, lRow+5]  := Format('%6.1f', [lSourceSplit.XStdDeviation]);
                      GaugeGrid.Cells[9, lRow+5]  := Format('%4.1f', [lSourceSplit.XCV]);
                    end;
                    if ((lSourceSplit.HydroEndYear = 0) AND (lSourceSplit.HydroStartYear = 0)) then
                      lValue := 0
                    else
                      lValue := (lSourceSplit.HydroEndYear - lSourceSplit.HydroStartYear + 1);
                    GaugeGrid.Cells[11, lRow+5] := Format('%4d', [Trunc(lValue)]);
                    if (lValue <> 0) then
                      lValue := lSourceSplit.NrOfMissingMonths * 100 / (lValue * 12);
                    GaugeGrid.Cells[10, lRow+5] := Format('%6.2f', [lValue]);
                    if (lSourceStation.Height <> NullInteger) then
                      GaugeGrid.Cells[12, lRow+5] := Format('%4d', [lSourceStation.Height])
                    else
                      GaugeGrid.Cells[12, lRow+5] := '';
                    GaugeGrid.Cells[13, lRow+5] := lSourceStation.StationType;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.PopulateGraph;
const OPNAME = 'TRainfallPatchAdminValidator.PopulateGraph';
var
  LIndex            : integer;
  lSeriesID         : integer;
  lSeriesName       : string;
  lStartDate        : TDateTime;
  lEndDate          : TDateTime;
  lRainfallObj      : IRainfallModelData;
  lStation          : IStationData;
  lPatch            : IPatchData;
  lSourcePatchID    : integer;
  lSourceStationID  : integer;
  lTargetStation    : WideString;
  lSourceStation    : IStationData;
  lSourcePatch      : IPatchData;
  lCount            : integer;
  lStartYear        : integer;
  lEndYear          : integer;
begin
  try
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    with RainfallPatchAdminDialog do
    begin
      ChartSeries.Clear;
      if (FCurrentPatchID = 0) then
      begin
        RecordLengthChart.Title.Text.Text := FAppModules.Language.GetString('Rainfall.RecordLengthProjectGauges');
        RecordLengthChart.Title.Visible := True;
        for LIndex := 0 to lRainfallObj.StationCount - 1 do
        begin
          lStation := lRainfallObj.GetStationDataByIndex(LIndex);
          if (lStation <> nil) then
          begin
            lSeriesName := lStation.RainfallData.StationNumber;
            lSeriesID   := lStation.RainfallData.StationID;
            AddSeriesWithGapsToChart(LIndex, lSeriesID, lSeriesName, clYellow, 0, 0, clYellow);
          end;
        end;
      end
      else
      begin
        lStation := lRainfallObj.GetStationDataByID(FCurrentStationID);
        if (lStation <> nil) then
        begin
          lPatch := lStation.GetPatchWithID(FCurrentPatchID);
          if (lPatch <> nil) then
          begin
            RecordLengthChart.Title.Text.Text := FAppModules.Language.GetString('Rainfall.RecordLengthFor') + lPatch.PatchName;
            RecordLengthChart.Title.Visible := True;
            lSeriesName := lPatch.PatchName;
            LStartDate  := lPatch.RainfallData.HydroStartDate;
            LEndDate    := lPatch.RainfallData.HydroEndDate;
            AddSeriesToChart(ChartSeries, LStartDate, LEndDate, -1, lSeriesName, clLime);
            if (lPatch.PatchTypeID = 1) then
            begin
              lSeriesID   := lStation.RainfallData.StationID;
              lSeriesName := lStation.RainfallData.StationNumber;
              AddSeriesWithGapsToChart (0, lSeriesID, lSeriesName, clBlue, 0, 0, clGray);
            end
            else
            begin
              lCount := 0;
              for LIndex := 0 to lPatch.SourcesCount - 1 do
              begin
                lPatch.GetSourceInfoByIndex(lIndex, lSourceStationID, LSourcePatchID,
                                            lTargetStation, lStartYear, lEndYear);
                lSourceStation := lRainfallObj.GetStationDataByID(lSourceStationID);
                if (lSourceStation <> nil) then
                begin
                  lSeriesID := lSourceStation.RainfallData.StationID;
                  if (lTargetStation = 'Y') then
                  begin
                    lSeriesName := lSourceStation.RainfallData.StationNumber;
                    AddSeriesWithGapsToChart(0, lSeriesID, lSeriesName, clBlue, lStartYear, lEndYear, clGray);
                  end
                  else
                  begin
                    lCount := lCount + 1;
                    if (LSourcePatchID = 0) then
                    begin
                      lSeriesName := lSourceStation.RainfallData.StationNumber;
                      AddSeriesWithGapsToChart(lCount, lSourceStationID, lSeriesName, clYellow, lStartYear, lEndYear, clGray);
                    end
                    else
                    begin
                      lSourcePatch  := lSourceStation.GetPatchWithID(LSourcePatchID);
                      lSeriesName   := lSourcePatch.PatchName;
                      LStartDate    := lSourcePatch.RainfallData.HydroStartDate;
                      LEndDate      := lSourcePatch.RainfallData.HydroEndDate;
                      AddSeriesToChart(ChartSeries, LStartDate, LEndDate, lCount, lSeriesName, clBlack);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.AddSeriesWithGapsToChart (AIndex       : integer;
                                                                 AStationID   : integer;
                                                                 AStationName : string;
                                                                 AColor       : TColor;
                                                                 AStartYear   : integer;
                                                                 AEndYear     : integer;
                                                                 ADullColor   : TColor);
const OPNAME = 'TRainfallPatchAdminValidator.AddSeriesWithGapsToChart';
var
  lStartDate        : TDateTime;
  lEndDate          : TDateTime;
  lStation          : IStationData;
  lNrOfHydroYears   : integer;
  lInGap            : Boolean;
  lMonthIndex       : integer;
  lYearIndex        : integer;
  lMonthValue       : double;
  lYearlyData       : IYearlyData;
  lSplitStartDate   : TDateTime;
  lSplitEndDate     : TDateTime;
begin
  try
    with RainfallPatchAdminDialog do
    begin
      lSplitStartDate := 0;
      lSplitEndDate  := 0;
      if (AStartYear <> 0) then
        lSplitStartDate := EncodeDate(AStartYear, 10, 1);
      if (AEndYear <> 0) then
        lSplitEndDate := EncodeDate(AEndYear, 9, 30);
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(AStationID);
      if (lStation <> nil) then
      begin
        lStartDate      := lStation.RainfallData.HydroStartDate;
        lNrOfHydroYears := lStation.RainfallData.HydroYearsCount;
        lInGap          := TRUE;
        for lYearIndex := 0 to lNrOfHydroYears - 1 do
        begin
          lYearlyData := lStation.RainfallData.GetHydroYearDataByIndex(lYearIndex);
          for lMonthIndex := 1 to 12 do
          begin
            lMonthValue := lYearlyData.MonthlyRainfall[lMonthIndex];
            if (lMonthValue = NullFloat) then
            begin
              if (NOT lInGap) then
              begin
                lInGap := TRUE;
                if (lMonthIndex <= 3) then
                  lEndDate := EncodeDate(lYearlyData.Year, lMonthIndex+9, 1)
                else
                  lEndDate := EncodeDate(lYearlyData.Year+1, lMonthIndex-3, 1);
                AddSplitSeriesToChart(lStartDate, lEndDate, AIndex, AStationName, AColor,
                                      lSplitStartDate, lSplitEndDate, ADullColor);
              end;
            end
            else
            begin
              if (lInGap) then
              begin
                lInGap := FALSE;
                if (lMonthIndex <= 3) then
                  lStartDate := EncodeDate(lYearlyData.Year, lMonthIndex+9, 1)
                else
                  lStartDate := EncodeDate(lYearlyData.Year+1, lMonthIndex-3, 1);
              end;
              if ((lYearIndex = lNrOfHydroYears - 1) AND (lMonthIndex = 12)) then
              begin
                lEndDate := lStation.RainfallData.HydroEndDate;
                AddSplitSeriesToChart(lStartDate, lEndDate, AIndex, AStationName, AColor,
                                      lSplitStartDate, lSplitEndDate, ADullColor);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminValidator.AddSplitSeriesToChart (AStartDate      : TDateTime;
                                                              AEndDate        : TDateTime;
                                                              AIndex          : integer;
                                                              AStationName    : string;
                                                              AColor          : TColor;
                                                              ASplitStartDate : TDateTime;
                                                              ASplitEndDate   : TDateTime;
                                                              ADullColor      : TColor);
const OPNAME = 'TRainfallPatchAdminValidator.AddSplitSeriesToChart';
begin
  try
    with RainfallPatchAdminDialog do
    begin
      if (ASplitStartDate = 0) AND (ASplitEndDate = 0) then
        AddSeriesToChart(ChartSeries, AStartDate, AEndDate, AIndex, AStationName, AColor)
      else
      begin
        if (AStartDate < ASplitStartDate) then
        begin
          if (AEndDate <= ASplitStartDate) then
            AddSeriesToChart(ChartSeries, AStartDate, AEndDate, AIndex, AStationName, ADullColor)
          else
          if (AEndDate <= ASplitEndDate) then
          begin
            AddSeriesToChart(ChartSeries, AStartDate, ASplitStartDate, AIndex, AStationName, ADullColor);
            AddSeriesToChart(ChartSeries, ASplitStartDate, AEndDate, AIndex, AStationName, AColor);
          end
          else
          begin
            AddSeriesToChart(ChartSeries, AStartDate, ASplitStartDate, AIndex, AStationName, ADullColor);
            AddSeriesToChart(ChartSeries, ASplitStartDate, ASplitEndDate, AIndex, AStationName, AColor);
            AddSeriesToChart(ChartSeries, ASplitEndDate, AEndDate, AIndex, AStationName, ADullColor);
          end;
        end
        else
        if (AStartDate < ASplitEndDate) then
        begin
          if (AEndDate <= ASplitEndDate) then
          begin
            AddSeriesToChart(ChartSeries, AStartDate, AEndDate, AIndex, AStationName, AColor);
          end
          else
          begin
            AddSeriesToChart(ChartSeries, AStartDate, ASplitEndDate, AIndex, AStationName, AColor);
            AddSeriesToChart(ChartSeries, ASplitEndDate, AEndDate, AIndex, AStationName, ADullColor);
          end;
        end
        else
        begin
          AddSeriesToChart(ChartSeries, AStartDate, AEndDate, AIndex, AStationName, ADullColor);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallPatchAdminValidator.AddSeriesToChart (ASeries      : TGanttSeries;
                                                        AStartDate   : TDateTime;
                                                        AEndDate     : TDateTime;
                                                        AIndex       : Integer;
                                                        ADescription : string;
                                                        AColor       : TColor): boolean;
const OPNAME = 'TRainfallPatchAdminValidator.AddSeriesToChart';
begin
  Result := False;
  try
    if AStartDate < EncodeDate(1800,2,1) then
      AStartDate  := EncodeDate(1990, 1, 1);
    if AEndDate < EncodeDate(1800,2,1) then
      AEndDate  := EncodeDate(1990, 1, 1);
    ASeries.AddGanttColor(AStartDate, AEndDate, AIndex, ADescription, AColor);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoPlotSplitterMoved(Sender: TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoPlotSplitterMoved';
begin
  try
    with RainfallPatchAdminDialog do
    begin
      if (PanelGrid.Height < GaugeGrid.RowHeights[0]) then
        PanelGrid.Height := GaugeGrid.RowHeights[0];
      if ((NOT FSystemFlag) AND (RecordLengthChart.Height < PanelClient.Height) AND
        FShowGrid AND FShowGraph) then
      begin
        FGraphRatio := PanelChart.Height /
                       (PanelClient.Height - PanelButton.Height - HorSplitter.Height);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminValidator.DisplayGridGraph;
const OPNAME = 'TRainfallPatchAdminValidator.DisplayGridGraph';
begin
  try
    FSystemFlag := TRUE;
    with RainfallPatchAdminDialog do
    begin
      PanelGrid.Visible   := FShowGrid;
      HorSplitter.Visible := FShowGrid AND FShowGraph;
      PanelChart.Visible  := FShowGraph;
      if (FShowGrid) then
        FAppModules.SetMenuItem(CToggleGrid, msChecked)
      else
        FAppModules.SetMenuItem(CToggleGrid, msUnChecked);
      if (FShowGraph) then
        FAppModules.SetMenuItem(CToggleGraph, msChecked)
      else
        FAppModules.SetMenuItem(CToggleGraph, msUnChecked);
      if (PanelGrid.Visible) then
      begin
        if (NOT PanelChart.Visible) then
          PanelGrid.Align := alClient
        else
        begin
          PanelGrid.Align  := alTop;
          PanelGrid.Height := Trunc((PanelClient.Height - PanelButton.Height - HorSplitter.Height) *
                                    (1 - FGraphRatio));
        end;
      end;
      if (PanelChart.Visible) then
        PanelChart.Align := alClient;
      PanelGrid.Top   := 25;
      HorSplitter.Top := PanelGrid.Top + PanelGrid.Height;
      PanelChart.Top  := HorSplitter.Top + HorSplitter.Height;
    end;
    FSystemFlag := FALSE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminValidator.DoToggleGrid(ASender : TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoToggleGrid';
begin
  try
    FShowGrid := NOT FShowGrid;
    DisplayGridGraph;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoToggleGraph(ASender : TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoToggleGraph';
begin
  try
    FShowGraph := NOT FShowGraph;
    DisplayGridGraph;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoToggleTree;
const OPNAME = 'TRainfallPatchAdminValidator.DoToggleTree';
begin
  try
    FShowTree := NOT FShowTree;
    with RainfallPatchAdminDialog do
    begin
      PanelTreeView.Visible := FShowTree;
      PanelLeft.Visible     := FShowTree;
      Splitter.Left         := PanelLeft.Left + (PanelLeft.Width + 1);
    end;
    if FShowTree then
      FAppModules.SetMenuItem(CToggleTree, msChecked)
    else
      FAppModules.SetMenuItem(CToggleTree, msUnChecked);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoDrawGrid (Sender : TObject;
                                                   ACol   : integer;
                                                   ARow   : Integer;
                                                   Rect   : TRect;
                                                   State  : TGridDrawState);
const OPNAME = 'TRainfallPatchAdminValidator.DoDrawGrid';
var
  LRect          : TRect;
  LOldColour     : TColor;
  LOldBrushStyle : TBrushStyle;
begin
  try
    with RainfallPatchAdminDialog do
    begin
      if (ARow in [2,4]) AND (GaugeGrid.RowHeights[ARow] <= 8) then
      begin
        LOldColour := GaugeGrid.Canvas.Brush.Color;
        LOldBrushStyle := GaugeGrid.Canvas.Brush.Style;

        try
          LRect.Left := Rect.Left + 0;
          LRect.Top := Rect.Top + 0;
          LRect.Right := Rect.Right - 0;
          LRect.Bottom := Rect.Bottom - 0;
          GaugeGrid.Canvas.Brush.Color := clGray;
          GaugeGrid.Canvas.Brush.Style := bsSolid;
          GaugeGrid.Canvas.FillRect(LRect);
        finally
          GaugeGrid.Canvas.Brush.Style := LOldBrushStyle;
          GaugeGrid.Canvas.Brush.Color := LOldColour;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRainfallPatchAdminValidator.CanPrint: Boolean;
const OPNAME = 'TRainfallPatchAdminValidator.CanPrint';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallPatchAdminValidator.CanCopyToClipboard: Boolean;
const OPNAME = 'TRainfallPatchAdminValidator.CanCopyToClipboard';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallPatchAdminValidator.CanExport: Boolean;
const OPNAME = 'TRainfallPatchAdminValidator.CanExport';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoPrint;
const OPNAME = 'TRainfallPatchAdminValidator.DoPrint';
var
  lPrinterOrientation : TPrinterOrientation;
  lGridChecked        : boolean;
begin
  try
    if (RainfallPatchAdminDialog.GaugeTreeview.Selected <> nil) then
    begin
      lPrinterOrientation := Printer.Orientation;
      try
        Printer.Orientation := poLandscape;
        if (FShowGrid) then
          RainfallPatchAdminDialog.GaugeGrid.DoPrint(FAppModules.Language.GetString('Rainfall.PatchAdministration'));
        if (FShowGraph) then
        begin
          lGridChecked := FShowGrid;
          if (lGridChecked) then
            DoToggleGrid(Self);
          RainfallPatchAdminDialog.Repaint;
          RainfallPatchAdminDialog.RecordLengthChart.DoPrint;
          if (lGridChecked) then
            DoToggleGrid(Self);
        end;
      finally
        Printer.Orientation := lPrinterOrientation;
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Message.SelectInTreeview'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoCopyToClipboard;
const OPNAME = 'TRainfallPatchAdminValidator.DoCopyToClipboard';
begin
  try
    if (RainfallPatchAdminDialog.GaugeTreeview.Selected <> nil) then
    begin
      if (FShowGrid AND FShowGraph) then
        ShowMessage(FAppModules.Language.GetString('Message.DisplayGraphOrGrid'))
      else
      if (FShowGrid) then
        RainfallPatchAdminDialog.GaugeGrid.DoCopyToClipboard
      else
      if (FShowGraph) then
        RainfallPatchAdminDialog.RecordLengthChart.DoCopyToClipboard;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Message.SelectInTreeview'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallPatchAdminValidator.DoExport';
var
  lGridChecked : boolean;
begin
  try
    if (RainfallPatchAdminDialog.GaugeTreeview.Selected <> nil) then
    begin
      if (FShowGrid) then
        RainfallPatchAdminDialog.GaugeGrid.DoExport(AFileName);
      if (FShowGraph) then
      begin
        lGridChecked := FShowGrid;
        if (lGridChecked) then
          DoToggleGrid(Self);
        RainfallPatchAdminDialog.Repaint;
        RainfallPatchAdminDialog.RecordLengthChart.DoExport(AFileName);
        if (lGridChecked) then
          DoToggleGrid(Self);
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Message.SelectInTreeview'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoCreatePatch(ASender: TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoCreatePatch';
var
  LPatchDescription : string;
begin
  try
    with RainfallPatchAdminDialog do
    begin
      if (GaugeTreeview.Selected = nil) OR (GaugeTreeview.Selected.Level <> 1) then
      begin
        ShowMessage(FAppModules.Language.GetString('Message.SelectRawStation'));
      end
      else
      begin
        LPatchDescription := InputBox(Application.Title, FAppModules.Language.GetString('InputBox.EnterDescription'), '');
        if (LPatchDescription <> '') then
        begin
          CreateANewPatch(LPatchDescription, Integer(GaugeTreeview.Selected.Data), GaugeTreeview.Selected.Index);
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallPatchAdminValidator.DoDeletePatch(ASender: TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoDeletePatch';
begin
  try
    with RainfallPatchAdminDialog do
    begin
      if (GaugeTreeview.Selected = nil) or (GaugeTreeview.Selected.Level <> 2) then
      begin
        ShowMessage(FAppModules.Language.GetString('Message.SelectPatch'));
      end
      else
      begin
        FCurrentStationID := Integer(GaugeTreeview.Selected.Parent.Data);
        FCurrentPatchID   := 0;
        DeletePatch(Integer(GaugeTreeview.Selected.Data));
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallPatchAdminValidator.DoUpdatePatch(ASender: TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoUpdatePatch';
var
  LPatchDescription : string;
begin
  try
    with RainfallPatchAdminDialog do
    begin
      if (GaugeTreeview.Selected = nil) or (GaugeTreeview.Selected.Level <> 2) then
      begin
        ShowMessage(FAppModules.Language.GetString('Message.SelectPatch'));
      end
      else
      begin
        LPatchDescription := InputBox(Application.Title, FAppModules.Language.GetString('InputBox.EnterDescription'), GaugeTreeview.Selected.Text);
        if (LPatchDescription <> '') and (LPatchDescription <> GaugeTreeview.Selected.Text)  then
        begin
          UpdatePatch(Integer(GaugeTreeview.Selected.Parent.Data),
                      Integer(GaugeTreeview.Selected.Data),
                      LPatchDescription);
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRainfallPatchAdminValidator.CreateANewPatch (APatchDescription : string;
                                                       AStationID        : integer;
                                                       ASplitIndex       : integer): integer;
const OPNAME = 'TRainfallPatchAdminValidator.CreateANewPatch';
var
  lRainfallObj   : IRainfallModelData;
  lStation       : IStationData;
  lSplit         : IRainfallDataSplit;
begin
  Result := 0;
  try
    if Assigned(FAppModules) and Assigned(FAppModules.Model()) and Assigned(FAppModules.Model.ModelData()) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lStation     := lRainfallObj.GetStationDataByID(AStationID);
      if (lStation <> nil) then
      begin
        if (ASplitIndex = 0) then
          Result := lRainfallObj.CreateAPatch(2, AStationID, APatchDescription, 0, 0)
        else
        begin
          lSplit := lStation.GetSplitWithIndex(ASplitIndex);
          if (lSplit <> nil) then
            Result := lRainfallObj.CreateAPatch(2, AStationID, APatchDescription,
                                                lSplit.HydroStartYear, lSplit.HydroEndYear);
        end;
        if Result > 0 then
          PopulateTreeView;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallPatchAdminValidator.UpdatePatch (AStationID      : integer;
                                               APatchID        : integer;
                                               ANewDescription : string): boolean;
const OPNAME = 'TRainfallPatchAdminValidator.UpdatePatch';
begin
  Result := False;
  try
    if Assigned(FAppModules) and Assigned(FAppModules.Model()) and Assigned(FAppModules.Model.ModelData()) then
    begin
      if (FAppModules.Model.ModelData as IRainfallModelData).
           ModifyPatchDescription(AStationID, APatchID, ANewDescription) then
        PopulateTreeView;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallPatchAdminValidator.DeletePatch (APatchID : integer): boolean;
const OPNAME = 'TRainfallPatchAdminValidator.DeletePatch';
begin
  Result := False;
  try
    if Assigned(FAppModules) and Assigned(FAppModules.Model()) and Assigned(FAppModules.Model.ModelData()) then
    begin
      if (FAppModules.Model.ModelData as IRainfallModelData).
           DeleteAPatch(APatchID) then
        PopulateTreeView;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoSetAddGaugeMode(ASender : TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoSetAddGaugeMode';
begin
  try
    FAddGaugeNode := RainfallPatchAdminDialog.GaugeTreeview.Selected;
    ResetButtons;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoAddGauge(ASender: TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoAddGauge';
var
  lSourcePatchID     : integer;
  lSourceStationID   : integer;
  lSplitIndex        : integer;
  lEndYear           : integer;
  lStartYear         : integer;
  lPos               : integer;
  lNodeTxt           : string;
begin
  try
    lSourcePatchID   := 0;
    lSourceStationID := 0;
    with RainfallPatchAdminDialog do
    begin
      lStartYear       := 0;
      lEndYear         := 0;
      if (GaugeTreeview.Selected <> nil) then
      begin
        lSplitIndex := 0;
        if (GaugeTreeview.Selected.Level = 0) then
        begin
          lSourceStationID := Integer(GaugeTreeview.Selected.Data);
          lSplitIndex      := 0;
          lSourcePatchID   := 0;
        end
        else
        if (GaugeTreeview.Selected.Level = 1) then
        begin
          lSourceStationID := Integer(GaugeTreeview.Selected.Parent.Data);
          lSplitIndex      := GaugeTreeview.Selected.Index;
          lSourcePatchID   := 0;
          lNodeTxt         := Trim(GaugeTreeview.Selected.Text);
          lPos             := Pos('-', lNodeTxt);
          if (lPos > 0) then
          begin
            lStartYear := StrToInt(Trim(Copy(lNodeTxt, 1, lPos-1)));
            lEndYear   := StrToInt(Trim(Copy(lNodeTxt, lPos+1, Length(lNodeTxt)-lPos)));
          end;
        end
        else
        if (GaugeTreeview.Selected.Level = 2) then
        begin
          lSourceStationID := Integer(GaugeTreeview.Selected.Parent.Data);
          lSplitIndex      := GaugeTreeview.Selected.Parent.Index;
          lSourcePatchID   := Integer(GaugeTreeview.Selected.Data);
          lNodeTxt         := Trim(GaugeTreeview.Selected.Parent.Text);
          lPos             := Pos('-', lNodeTxt);
          if (lPos > 0) then
          begin
            lStartYear := StrToInt(Trim(Copy(lNodeTxt, 1, lPos-1)));
            lEndYear   := StrToInt(Trim(Copy(lNodeTxt, lPos+1, Length(lNodeTxt)-lPos)));
          end;
        end;

        if (lSourcePatchID = FCurrentPatchID) then
          ShowMessage(FAppModules.Language.GetString('Message.CanNotAddPatch'))
        else
        begin
          if (lSplitIndex = 0) then
            AddToPatch(lSourceStationID, lSourcePatchID, 0, 0)
          else
          begin
            AddToPatch(lSourceStationID, lSourcePatchID, lStartYear, lEndYear);
          end;
          PopulateTreeView;
        end;
      end
      else
        ShowMessage(FAppModules.Language.GetString('Message.AddGaugeTo'));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.DoRemoveGauge(ASender: TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoRemoveGauge';
var
  LSourcePatchID   : integer;
  LSourceStationID : integer;
begin
  try
    with RainfallPatchAdminDialog do
    begin
      if GaugeGrid.Row < 5 then
        ShowMessage(FAppModules.Language.GetString('Message.SelectSourceStation'))
      else
      begin
        if (FCurrentPatchID <> 0) then
        begin
          LSourcePatchID   := Integer(GaugeGrid.Objects[1,GaugeGrid.Row]);
          LSourceStationID := Integer(GaugeGrid.Objects[0,GaugeGrid.Row]);
          if (LSourceStationID <> 0) then
            DeleteFromPatch(FCurrentPatchID, LSourceStationID, LSourcePatchID);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallPatchAdminValidator.AddToPatch (ASourceStationID  : Integer;
                                                  ASourcePatchID    : Integer;
                                                  AStartYear        : integer;
                                                  AEndYear          : integer): Boolean;
const OPNAME = 'TRainfallPatchAdminValidator.AddToPatch';
var
  lPatch         : IPatchData;
  lStation       : IStationData;
  lIndex         : integer;
  lStationID     : integer;
  lSourceStation : IStationData;
  lPatchID       : integer;
  lTargetStation : WideString;
  lDuplicate     : boolean;
  lMessage       : string;
  lRainfallObj   : IRainfallModelData;
  lFileName      : string;
  lNewFileName   : string;
  lStartYear     : integer;
  lEndYear       : integer;
begin
  Result := False;
  try
    if Assigned(FAppModules) and Assigned(FAppModules.Model()) and Assigned(FAppModules.Model.ModelData()) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lStation := lRainfallObj.GetStationDataByID(FCurrentStationID);
      if (lStation <> nil) then
      begin
        lPatch := lStation.GetPatchWithID(FCurrentPatchID);
        if (lPatch <> nil) then
        begin
          lDuplicate := FALSE;
          lIndex := 0;
          while ((NOT lDuplicate) AND (lIndex < lPatch.SourcesCount)) do
          begin
            lPatch.GetSourceInfoByIndex(lIndex, lStationID, lPatchID,
                                        lTargetStation, lStartYear, lEndYear);
            lSourceStation := lRainfallObj.GetStationDataByID(lStationID);
            lFileName := lSourceStation.RainfallData.StationNumber;
            while (Pos(' ', lFileName) > 0) do
              Delete(lFileName, Pos(' ', lFileName), 1);
            lFileName := Copy(lFileName, 1, 8);

            if (lStationID = ASourceStationID) then
            begin
              lDuplicate := TRUE;
              lMessage := FAppModules.Language.GetString('Rainfall.PatchContainSource');
            end
            else
            if (lFileName = lNewFileName) then
            begin
              lDuplicate := TRUE;
              lMessage := FAppModules.Language.GetString('Rainfall.FileNameConflict') +
                          lSourceStation.RainfallData.StationNumber;
            end
            else
              lIndex := lIndex + 1;
          end;
          if (NOT lDuplicate )then
          begin
            lSourceStation := lRainfallObj.GetStationDataByID(ASourceStationID);
            Result := (FAppModules.Model.ModelData as IRainfallModelData).
                      AddToPatch(FCurrentStationID, FCurrentPatchID, ASourceStationID,
                                 ASourcePatchID, AStartYear, AEndYear);
            lSourceStation.LoadMonthlyData;
          end
          else
            ShowMessage(lMessage);
        end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallPatchAdminValidator.DeleteFromPatch (APatchID         : integer;
                                                       ASourceStationID : integer;
                                                       ASourcePatchID   : integer): Boolean;
const OPNAME = 'TRainfallPatchAdminValidator.DeleteFromPatch';
begin
  Result := False;
  try
    if Assigned(FAppModules) and Assigned(FAppModules.Model()) and Assigned(FAppModules.Model.ModelData()) then
    begin
      Result := (FAppModules.Model.ModelData as IRainfallModelData).
                  RemoveFromPatch(APatchID, ASourceStationID, ASourcePatchID);
      PopulateTreeView;
      DoGaugeTreeViewChanged(Self, RainfallPatchAdminDialog.GaugeTreeview.Selected);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallPatchAdminValidator.SetPopupRemoveClick ( Sender: TObject );
const OPNAME = 'TRainfallPatchAdminValidator.SetPopupRemoveClick';
begin
  try
    DoDeletePatch(nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminValidator.SetPopupRenameClick ( Sender: TObject );
const OPNAME = 'TRainfallPatchAdminValidator.SetPopupRenameClick';
begin
  try
    DoUpdatePatch(nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminValidator.DoGetAxisLabel (Sender         : TChartAxis;
                                                       ASeries        : TChartSeries;
                                                       AValueIndex    : integer;
                                                       var ALabelText : String );
const OPNAME = 'TRainfallPatchAdminValidator.DoGetAxisLabel';

begin
  try
    if (Sender = RainfallPatchAdminDialog.RecordLengthChart.BottomAxis) then
      aLabelText := FormatDateTime('yyyy/mm', StrToFloat(aLabelText));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallPatchAdminValidator.SaveState: Boolean;
const OPNAME = 'TRainfallPatchAdminValidator.SaveState';
begin
 Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallPatchAdminValidator.DoIncludeUnreliableCheckBoxClick (Sender : TObject);
const OPNAME = 'TRainfallPatchAdminValidator.DoIncludeUnreliableCheckBoxClick';
var
  lRainfallObj : IRainfallModelData;
begin
  try
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lRainfallObj.IncludeUnreliableData := RainfallPatchAdminDialog.IncludeUnreliableChkBox.Checked;
    PopulateGrid;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.



