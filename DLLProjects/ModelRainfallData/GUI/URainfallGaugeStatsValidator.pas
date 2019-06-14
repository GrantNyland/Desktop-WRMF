
{******************************************************************************}
{*  UNIT      : Contains TRainfallGaugeStatsValidator Class                   *}
{*  AUTHOR    : RH Steyn(Cornastone)                                          *}
{*  DATE      : 29/04/2005                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallGaugeStatsValidator;

interface
{$WARN UNIT_PLATFORM OFF}
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
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  Contnrs,
  UUtilities,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  URainfallGaugeStatsMenuItemManager,
  UMenuItemManager,
  UDataComponent,
  URainfallGaugeStatsDialog,
  UGenericModelLinkClasses;

type

  TRainfallGaugeStatsValidator = class (TAbstractDataDialogValidator)
  protected
    FPopulateFlag          : Boolean;
    FCurrentStationID      : integer;
    FCurrentSplitIndex     : integer;
    FCurrentPatchID        : integer;
    FGraphStationID        : integer;
    FGraphSplitIndex       : integer;
    FGraphPatchID          : integer;
    FGraphRatio            : double;
    FSystemFlag            : boolean;
    FShowGrid              : boolean;
    FShowGraph             : boolean;
    FShowTree              : boolean;
    FMenuItemManager       : TRainfallGaugeStatsMenuItemManager;
    FNode                  : TTreeNode;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function RainfallGaugeStatsDialog : TRainfallGaugeStatsDialog;
    procedure DoGaugeTreeViewChanged (ASender: TObject; ANode: TTreeNode);
    procedure DoTreeViewChanging (ASender : TObject;
                                  ANode   : TTreeNode;
                                  var AllowChange : Boolean);
    procedure DisplayGridGraph (Sender: TObject);
    procedure DoPlotSplitterMoved (Sender: TObject);
    procedure DoDrawGraph (Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
    procedure DoCreateFiles(Sender: TObject);
    procedure DoGraphTypeClick (Sender: TObject);
    procedure DoIncludeUnreliableCheckBoxClick ( Sender : TObject );
    procedure ClearGrid;
    procedure PopulateGrid;
    procedure PopulateGraph (ARow   : integer;
                             AForce : boolean=FALSE);
    procedure RePopulateDataViewer;
    procedure PopulateTreeView;
    procedure ResetButtons;
  public
    function Initialise: boolean; override;
    procedure PopulateDataViewer; override;
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
    procedure DoToggleGrid(ASender : TObject);
    procedure DoToggleGraph(ASender : TObject);
    procedure DoToggleTree(ASender : TObject);
    procedure OnClickCreateFiles(Sender: TObject);
    procedure DoCreateSplit(Sender: TObject);
    procedure DoUpdateSplit(Sender: TObject);
    procedure DoDeleteSplit(Sender: TObject);
    property MenuItemManager : TRainfallGaugeStatsMenuItemManager read FMenuItemManager write FMenuItemManager;
  end;

implementation

uses
  SysUtils,
  VCL.ImgList,
  VCL.FileCtrl,
  UDatasetType,
  VCL.Printers,
  UConstants,
  RainfallCom_TLB,
  USplitRainfallRecordForm,
  UUpdateSplitForm,
  UErrorHandlingOperations, URainfallCommonGaugeSheet;

{ TRainfallGaugeStatsValidator }

procedure TRainfallGaugeStatsValidator.CreateMemberObjects;
const OPNAME = 'TRainfallGaugeStatsValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TRainfallGaugeStatsDialog.Create(nil, FAppModules);

    with RainfallGaugeStatsDialog do
    begin
      HorSplitter.OnMoved             := DoPlotSplitterMoved;
      GraphType1RadioButton.OnClick   := DoGraphTypeClick;
      GraphType2RadioButton.OnClick   := DoGraphTypeClick;
      GaugeTreeview.OnChange          := DoGaugeTreeViewChanged;
      GaugeGrid.OnSelectCell          := DoDrawGraph;
      IncludeUnreliableChkBox.OnClick := DoIncludeUnreliableCheckBoxClick;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRainfallGaugeStatsValidator.DestroyMemberObjects;
const OPNAME = 'TRainfallGaugeStatsValidator.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeStatsValidator.Initialise: boolean;
const OPNAME = 'TRainfallGaugeStatsValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FGraphRatio        := 0.5;
    FSystemFlag        := FALSE;
    FPopulateFlag      := TRUE;
    FShowGrid          := TRUE;
    FShowGraph         := TRUE;
    FShowTree          := TRUE;
    FCurrentStationID  := 0;
    FCurrentSplitIndex := -1;
    FCurrentPatchID    := 0;
    FGraphStationID    := 0;
    FGraphSplitIndex   := -1;
    FGraphPatchID      := 0;
    if (FMenuItemManager <> nil) then
      FMenuItemManager.Initialise;
      
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGaugeStatsValidator.RainfallGaugeStatsDialog : TRainfallGaugeStatsDialog;
const OPNAME = 'TRainfallGaugeStatsValidator.RainfallGaugeStatsDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TRainfallGaugeStatsDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeStatsValidator.StudyHasChanged: boolean;
const OPNAME = 'TRainfallGaugeStatsValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGaugeStatsValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                           AFieldName : string;
                                                           AOldValue  : string;
                                                           ANewValue  : string): Boolean;
const OPNAME = 'TRainfallGaugeStatsValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGaugeStatsValidator.PopulateDataViewer;
const OPNAME = 'TRainfallGaugeStatsValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.RePopulateDataViewer;
const OPNAME = 'TRainfallGaugeStatsValidator.RePopulateDataViewer';
begin
  try
    with RainfallGaugeStatsDialog do
    begin
      PanelGrid.Height := Trunc((PanelClient.Height - PanelButton.Height - HorSplitter.Height) *
                                 (1 - FGraphRatio));
    end;
    FCurrentStationID  := (FAppModules.Model.ModelData as IRainfallModelData).CurrentStationID;
    FCurrentSplitIndex := (FAppModules.Model.ModelData as IRainfallModelData).CurrentSplitIndex;
    FCurrentPatchID    := (FAppModules.Model.ModelData as IRainfallModelData).CurrentPatchID;
    RainfallGaugeStatsDialog.IncludeUnreliableChkBox.Checked :=
      (FAppModules.Model.ModelData as IRainfallModelData).IncludeUnreliableData;
    PopulateTreeView;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.PopulateTreeView;
const OPNAME = 'TRainfallGaugeStatsValidator.PopulateTreeView';
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
    with RainfallGaugeStatsDialog do
    begin
      FPopulateFlag := FALSE;
      GaugeTreeview.Items.Clear;
      lSelectedNode := nil;
      lRainfallObj  := (FAppModules.Model.ModelData as IRainfallModelData);
      if (lRainfallObj.StationCount = 0) then
      begin
        IncludeUnreliableChkBox.Enabled := FALSE;
        PanelGraph.Enabled    := FALSE;
        GaugeGrid.Visible     := FALSE;
        RainfallGraph.Visible := FALSE;
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
          IncludeUnreliableChkBox.Enabled := TRUE;
          PanelGraph.Enabled    := TRUE;
          GaugeGrid.Visible     := TRUE;
          RainfallGraph.Visible := TRUE;
          GaugeTreeview.Selected := lSelectedNode;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.DoTreeViewChanging (ASender : TObject;
                                                           ANode   : TTreeNode;
                                                           var AllowChange : Boolean);
const OPNAME = 'TRainfallGaugeStatsValidator.DoTreeViewChanging';
begin
  try
    AllowChange := ANode.Level > 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.ResetButtons;
const OPNAME = 'TRainfallGaugeStatsValidator.ResetButtons';
begin
  try
    if (FMenuItemManager <> nil) then
    begin
      with RainfallGaugeStatsDialog do
      begin
        FMenuItemManager.SetCreateSplit((GaugeTreeview.SelectionCount = 1) AND
                                        (GaugeTreeview.Selected.Level = 0));
        FMenuItemManager.SetDeleteSplit((GaugeTreeview.SelectionCount = 1) AND
                                        (GaugeTreeview.Selected.Index <> 0) AND
                                        (GaugeTreeview.Selected.Level = 1));
        FMenuItemManager.SetUpdateSplit((GaugeTreeview.SelectionCount = 1) and
                                        (GaugeTreeview.Selected.Level = 1));

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.DoGaugeTreeViewChanged (ASender : TObject;
                                                               ANode   : TTreeNode);
const OPNAME = 'TRainfallGaugeStatsValidator.DoGaugeTreeViewChanged';
var
  lRainfallModelData : IRainfallModelData;
begin
  try
    if (FPopulateFlag) and (Assigned(FAppModules.Model())) and (Assigned(FAppModules.Model.ModelData())) then
    begin
      lRainfallModelData := (FAppModules.Model.ModelData as IRainfallModelData);
      with RainfallGaugeStatsDialog do
      begin
        PanelGrid.Height := Trunc((PanelClient.Height - PanelButton.Height - HorSplitter.Height) *
                                   (1 - FGraphRatio));
        if (GaugeTreeview.SelectionCount = 1) then
        begin
          FNode := ANode;
          if (GaugeTreeview.Selected.Level = 0) then
          begin
            FCurrentStationID  := Integer(FNode.Data);
            FCurrentSplitIndex := 0;
            FCurrentPatchID    := 0;
          end
          else
          if (GaugeTreeview.Selected.Level = 1) then
          begin
            FCurrentStationID  := Integer(FNode.Data);
            FCurrentSplitIndex := FNode.Index;
            FCurrentPatchID    := 0;
          end
          else
          if (GaugeTreeview.Selected.Level = 2) then
          begin
            FCurrentStationID  := Integer(ANode.Parent.Data);
            FCurrentSplitIndex := ANode.Parent.Index;
            FCurrentPatchID    := Integer(ANode.Data);
          end;
        end;
        lRainfallModelData.CurrentPatchID    := FCurrentPatchID;
        lRainfallModelData.CurrentSplitIndex := FCurrentSplitIndex;
        lRainfallModelData.CurrentStationID  := FCurrentStationID;
        ResetButtons;
        PopulateGrid;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.ClearGrid;
const OPNAME = 'TRainfallGaugeStatsValidator.ClearGrid';
var
  LIndex    : integer;
  LRowIndex : integer;
begin
  try
    with RainfallGaugeStatsDialog do
    begin
      for LRowIndex := 1 to GaugeGrid.RowCount- 1 do
      begin
        for LIndex := 0 to GaugeGrid.ColCount - 1 do
        begin
          GaugeGrid.Cells[LIndex, LRowIndex] := '';
          GaugeGrid.Objects[LIndex, LRowIndex] := nil;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.PopulateGrid;
const OPNAME = 'TRainfallGaugeStatsValidator.PopulateGrid';
var
  lIndex           : integer;
  lStationID       : integer;
  lPatchID         : integer;
  lSplitIndex      : integer;
  lRainfallData    : IRainfallModelData;
  lStation         : IStationData;
  lPatch           : IPatchData;
  lSplit           : IRainfallDataSplit;
  lValue           : double;
  lRow             : integer;
  lLatStr          : WideString;
  lLonStr          : WideString;
  lNode            : TTreeNode;
begin
  try
    if (FPopulateFlag) AND (Assigned(FAppModules.Model())) AND (Assigned(FAppModules.Model.ModelData())) then
    begin
      FSystemFlag := TRUE;
      ClearGrid;
      lRainfallData := (FAppModules.Model.ModelData as IRainfallModelData);
      with RainfallGaugeStatsDialog do
      begin
        GaugeGrid.RowCount := 1;
        lRow               := 0;
        for lIndex := 0 to GaugeTreeview.Items.Count - 1 do
        begin
          lNode := GaugeTreeview.Items[lIndex];
          if (lNode.Selected) then
          begin
            if (lNode.Level < 2) then
            begin
              lStationID := Integer(lNode.Data);
              lStation   := lRainfallData.GetStationDataByID(lStationID);
              if (lStation <> nil) then
              begin
                if (lNode.Level = 1) then
                  lSplit := lStation.GetSplitWithIndex(lNode.Index)
                else
                  lSplit := lStation.GetSplitWithIndex(0);
                lStation.LatLong(lLatStr, lLonStr);
                GaugeGrid.RowCount  := GaugeGrid.RowCount + 1;
                GaugeGrid.FixedRows := 1;
                lRow                := lRow + 1;
                GaugeGrid.Objects[0, lRow]:= TObject(lStation.RainfallData.StationID);
                if (lNode.Level = 0) then
                  GaugeGrid.Objects[1, lRow]:= TObject(0)
                else  
                  GaugeGrid.Objects[1, lRow]:= TObject(lNode.Index);
                GaugeGrid.Objects[2, lRow]:= nil;
                GaugeGrid.Cells[0, lRow]  := lStation.RainfallData.StationNumber;
                GaugeGrid.Cells[1, lRow]  := lStation.StationName;
                GaugeGrid.Cells[2, lRow]  := FAppModules.Language.GetString('Rainfall.RAW');
                GaugeGrid.Cells[3, lRow]  := lLatStr;
                GaugeGrid.Cells[4, lRow]  := lLonStr;
                GaugeGrid.Cells[5, lRow]  := IntToStr(lSplit.HydroStartYear);
                GaugeGrid.Cells[6, lRow]  := IntToStr(lSplit.HydroEndYear);
                if IncludeUnreliableChkBox.Checked then
                begin
                  GaugeGrid.Cells[7, lRow]  := Format('%6.1f', [lSplit.MAP]);
                  GaugeGrid.Cells[8, lRow]  := Format('%6.1f', [lSplit.StdDeviation]);
                  GaugeGrid.Cells[9, lRow]  := Format('%4.1f', [lSplit.CV]);
                end
                else
                begin
                  GaugeGrid.Cells[7, lRow]  := Format('%6.1f', [lSplit.XMAP]);
                  GaugeGrid.Cells[8, lRow]  := Format('%6.1f', [lSplit.XStdDeviation]);
                  GaugeGrid.Cells[9, lRow]  := Format('%4.1f', [lSplit.XCV]);
                end;
                if ((lSplit.HydroEndYear = 0) AND (lSplit.HydroStartYear = 0)) then
                  lValue := 0
                else
                  lValue := (lSplit.HydroEndYear - lSplit.HydroStartYear + 1);
                GaugeGrid.Cells[11, lRow] := Format('%4d', [Trunc(lValue)]);
                if (lValue <> 0) then
                  lValue := lSplit.NrOfMissingMonths * 100 / (lValue * 12);
                GaugeGrid.Cells[10, lRow] := Format('%6.2f', [lValue]);
                if (lStation.Height <> NullInteger) then
                  GaugeGrid.Cells[12, lRow] := Format('%4d', [lStation.Height])
                else
                  GaugeGrid.Cells[12, lRow] := '';
                GaugeGrid.Cells[13, lRow] := lStation.StationType;
              end;
            end
            else
            if (lNode.Level = 2) then
            begin
              lStationID := Integer(lNode.Parent.Data);
              lStation   := lRainfallData.GetStationDataByID(lStationID);
              lPatchID   := Integer(lNode.Data);
              lPatch     := lStation.GetPatchWithID(lPatchID);
              if (lPatch <> nil) then
              begin
                lStation.LatLong(lLatStr, lLonStr);
                GaugeGrid.RowCount  := GaugeGrid.RowCount + 1;
                GaugeGrid.FixedRows := 1;
                lRow               := lRow + 1;
                GaugeGrid.Objects[0, lRow]:= TObject(lStation.RainfallData.StationID);
                GaugeGrid.Objects[1, lRow]:= TObject(lNode.Index);
                GaugeGrid.Objects[2, lRow]:= TObject(lPatch.PatchID);
                GaugeGrid.Cells[0, lRow]  := lStation.RainfallData.StationNumber;
                GaugeGrid.Cells[1, lRow]  := lStation.StationName;
                GaugeGrid.Cells[2, lRow]  := lPatch.PatchName;
                GaugeGrid.Cells[3, lRow]  := lLatStr;
                GaugeGrid.Cells[4, lRow]  := lLonStr;
                GaugeGrid.Cells[5, lRow]  := IntToStr(lPatch.RainfallData.StartYear);
                GaugeGrid.Cells[6, lRow]  := IntToStr(lPatch.RainfallData.EndYear);
                if IncludeUnreliableChkBox.Checked then
                begin
                  GaugeGrid.Cells[7, lRow]  := Format('%6.1f', [lPatch.RainfallData.MAP]);
                  GaugeGrid.Cells[8, lRow]  := Format('%6.1f', [lPatch.RainfallData.StdDeviation]);
                  GaugeGrid.Cells[9, lRow]  := Format('%4.1f', [lPatch.RainfallData.CV]);
                end
                else
                begin
                  GaugeGrid.Cells[7, lRow]  := Format('%6.1f', [lPatch.RainfallData.XMAP]);
                  GaugeGrid.Cells[8, lRow]  := Format('%6.1f', [lPatch.RainfallData.XStdDeviation]);
                  GaugeGrid.Cells[9, lRow]  := Format('%4.1f', [lPatch.RainfallData.XCV]);
                end;

                if ((lPatch.RainfallData.EndYear = 0) AND (lPatch.RainfallData.StartYear = 0)) then
                  lValue := 0
                else
                  lValue := (lPatch.RainfallData.EndYear - lPatch.RainfallData.StartYear + 1);
                GaugeGrid.Cells[11, lRow] := Format('%4d', [Trunc(lValue)]);
                if (lValue <> 0) then
                  lValue := lPatch.RainfallData.NrOfMissingMonths * 100 / (lValue * 12);
                GaugeGrid.Cells[10, lRow] := Format('%6.2f', [lValue]);
                if (lStation.Height <> NullInteger) then
                  GaugeGrid.Cells[12, lRow] := Format('%4d', [lStation.Height])
                else
                  GaugeGrid.Cells[12, lRow] := '';
                GaugeGrid.Cells[13, lRow] := lStation.StationType;
              end;
            end
          end;
        end;
        FSystemFlag := FALSE;
        lRow := 0;
        lIndex := 1;
        while ((lRow = 0) AND (lIndex < GaugeGrid.RowCount)) do
        begin
          lStationID  := Integer(GaugeGrid.Objects[0, lIndex]);
          lSplitIndex := Integer(GaugeGrid.Objects[1, lIndex]);
          lPatchID    := Integer(GaugeGrid.Objects[2, lIndex]);
          if ((lStationID = FGraphStationID) AND (lSplitIndex =  FGraphSplitIndex) AND
              (lPatchID = FGraphPatchID)) then
            lRow := lIndex
          else
            lIndex := lIndex + 1;
        end;
        if (lRow > 0) then
          GaugeGrid.Row := lRow
        else if (GaugeGrid.RowCount > 1) then
          GaugeGrid.Row := 1;
        if (GaugeGrid.Row >= 1) then
          PopulateGraph(GaugeGrid.Row);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.PopulateGraph (ARow   : integer;
                                                      AForce : boolean=FALSE);
const OPNAME = 'TRainfallGaugeStatsValidator.PopulateGraph';
var
  lPrevStationID : integer;
  lPrevPatchID   : integer;
  lPrevSplitIdx  : integer;
  lStation       : IStationData;
  lPatch         : IPatchData;
  lSplit         : IRainfallDataSplit;
  lYearIndex     : integer;
  lYearlyData    : IYearlyData;
  lRainfallData  : IRainfallData;
  lStartYear     : integer;
  lEndYear       : integer;
  lCount         : integer;
  lCumTotal      : double;
  lQSumValue     : double;
  lSum           : double;
  lAvg           : double;
  lStartLabel    : string;
  lTitle         : string;
begin
  try
    lPrevStationID  := FGraphStationID;
    lPrevPatchID    := FGraphPatchID;
    lPrevSplitIdx   := FGraphSplitIndex;
    with RainfallGaugeStatsDialog do
    begin
      FGraphStationID  := Integer(GaugeGrid.Objects[0, ARow]);
      FGraphSplitIndex := Integer(GaugeGrid.Objects[1, ARow]);
      FGraphPatchID    := Integer(GaugeGrid.Objects[2, ARow]);
      if (AForce OR
         (FGraphStationID <> lPrevStationID) OR
         (FGraphSplitIndex <> lPrevSplitIdx) OR
         (FGraphPatchID <> lPrevPatchID)) then
      begin
        LineSeries.Clear;
        if (FGraphStationID > 0) then
        begin
          lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FGraphStationID);
          if (lStation <> nil) then
          begin
            lStartYear := 0;
            lEndYear   := 0;
            lSplit     := lStation.GetSplitWithIndex(FGraphSplitIndex);
            RainfallGraph.Title.Text.Clear;
            if (FGraphPatchID <> 0) then
            begin
              lPatch        := lStation.GetPatchWithID(FGraphPatchID);
              lRainfallData := lPatch.RainfallData;
              lStartYear    := lRainfallData.HydroStartYear;
              lEndYear      := lRainfallData.HydroEndYear;
              lTitle        := lStation.RainfallData.StationNumber + ' (' + lPatch.PatchName + ')';
              if (GraphType1RadioButton.Checked) then
                RainfallGraph.Title.Text.Add(lTitle + FAppModules.Language.GetString('Rainfall.CumulativeRainfall'))
              else
                RainfallGraph.Title.Text.Add(lTitle + FAppModules.Language.GetString('Rainfall.CumulativeDeviation'));
            end
            else
            begin
              lRainfallData := lStation.RainfallData;
              if (lSplit <> nil) then
              begin
                lStartYear := lSplit.HydroStartYear;
                lEndYear   := lSplit.HydroEndYear;
              end;
              lTitle := lStation.RainfallData.StationNumber + ' (' + IntToStr(lStartYear) + '-' + IntToStr(lEndYear) + ')';
              if (GraphType1RadioButton.Checked) then
                RainfallGraph.Title.Text.Add(lTitle + FAppModules.Language.GetString('Rainfall.CumulativeRainfall'))
              else
                RainfallGraph.Title.Text.Add(lTitle + FAppModules.Language.GetString('Rainfall.CumulativeDeviation'));
            end;
            if (lRainfallData <> nil) then
            begin
              lCount    := 0;
              lSum      := 0;
              lAvg      := 0;
              lCumTotal := 0;
              if (GraphType2RadioButton.Checked) then
              begin
                for lYearIndex :=  0 to lRainfallData.HydroYearsCount - 1 do
                begin
                  lYearlyData := lRainfallData.GetHydroYearDataByIndex(lYearIndex);
                  if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
                  begin
                    lCount := lCount + 1;
                    lSum   := lSum + lYearlyData.Total;
                  end;
                end;
                if (lCount > 0) then
                  lAvg := lSum / lCount;
                if lStartYear <> 0 then
                  lStartLabel := IntToStr(lStartYear - 1) + '/' +
                               Format('%2.2d', [StrToInt(Copy(IntToStr(lStartYear), 3, 2))])
                else
                  lStartLabel := IntToStr(0) + '/' +  Format('%2.2d', [0]);

                LineSeries.AddXY(0, 0, lStartLabel, clTeeColor);
                lCount := 0;
                for lYearIndex := 1 to lRainfallData.HydroYearsCount  do
                begin
                  lYearlyData := lRainfallData.GetHydroYearDataByIndex(lYearIndex-1);
                  if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
                  begin
                    lCount     := lCount + 1;
                    lCumTotal  := lCumTotal + lYearlyData.Total;
                    lQSumValue := lCumTotal - (lCount * lAvg);
                    LineSeries.AddXY(lCount, lQSumValue, lYearlyData.HydroYear, clTeeColor);
                  end;
                end;
              end
              else
              begin
                for lYearIndex := 0 to lRainfallData.HydroYearsCount - 1 do
                begin
                  lYearlyData := lRainfallData.GetHydroYearDataByIndex(lYearIndex);
                  if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
                  begin
                    lCumTotal   := lCumTotal + lYearlyData.Total;
                    LineSeries.AddXY(lYearIndex, lCumTotal, lYearlyData.HydroYear, clTeeColor);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsValidator.DoPlotSplitterMoved(Sender: TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.DoPlotSplitterMoved';
begin
  try
    with RainfallGaugeStatsDialog do
    begin
      if (PanelGrid.Height < GaugeGrid.RowHeights[0]) then
        PanelGrid.Height := GaugeGrid.RowHeights[0];
      if ((NOT FSystemFlag) AND (RainfallGraph.Height < PanelClient.Height) AND
        FShowGrid AND FShowGraph) then
      begin
        FGraphRatio := PanelGraph.Height /
                       (PanelClient.Height - PanelButton.Height - HorSplitter.Height);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsValidator.DisplayGridGraph(Sender: TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.DisplayGridGraph';
begin
  try
    FSystemFlag := TRUE;
    with RainfallGaugeStatsDialog do
    begin
      GraphType1RadioButton.Enabled := FShowGraph;
      GraphType2RadioButton.Enabled := FShowGraph;
      PanelGrid.Visible   := FShowGrid;
      HorSplitter.Visible := FShowGrid AND FShowGraph;
      PanelGraph.Visible  := FShowGraph;
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
        if (NOT PanelGraph.Visible) then
          PanelGrid.Align := alClient
        else
        begin
          PanelGrid.Align  := alTop;
          PanelGrid.Height := Trunc((PanelClient.Height - PanelButton.Height - HorSplitter.Height) *
                                    (1 - FGraphRatio));
        end;
      end;
      if (PanelGraph.Visible) then
        PanelGraph.Align := alClient;
      PanelGrid.Top   := 25;
      HorSplitter.Top := PanelGrid.Top + PanelGrid.Height;
      PanelGraph.Top  := HorSplitter.Top + HorSplitter.Height;
    end;
    FSystemFlag := FALSE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsValidator.DoToggleGrid(ASender : TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.DoToggleGrid';
begin
  try
    FShowGrid := NOT FShowGrid;
    DisplayGridGraph(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.DoToggleGraph(ASender : TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.DoToggleGraph';
begin
  try
    FShowGraph := NOT FShowGraph;
    DisplayGridGraph(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.DoDrawGraph (Sender        : TObject;
                                                    ACol, ARow    : Longint;
                                                    var CanSelect : Boolean);
const OPNAME = 'TRainfallGaugeStatsValidator.DoDrawGraph';
begin
  try
    with RainfallGaugeStatsDialog do
    begin
      if ((NOT FSystemFlag) AND (ACol >= GaugeGrid.FixedCols) AND (ARow >= GaugeGrid.FixedRows)) then
        PopulateGraph(ARow);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGaugeStatsValidator.CanPrint: Boolean;
const OPNAME = 'TRainfallGaugeStatsValidator.CanPrint';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGaugeStatsValidator.CanCopyToClipboard: Boolean;
const OPNAME = 'TRainfallGaugeStatsValidator.CanCopyToClipboard';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRainfallGaugeStatsValidator.CanExport: Boolean;
const OPNAME = 'TRainfallGaugeStatsValidator.CanExport';
begin
  Result := TRUE;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.DoPrint;
const OPNAME = 'TRainfallGaugeStatsValidator.DoPrint';
var
  lPrinterOrientation : TPrinterOrientation;
  lGridChecked        : Boolean;
begin
  try
    if (RainfallGaugeStatsDialog.GaugeTreeview.Selected <> nil) then
    begin
      lPrinterOrientation := Printer.Orientation;
      try
        Printer.Orientation := poLandscape;
        if (FShowGrid) then
          RainfallGaugeStatsDialog.GaugeGrid.DoPrint(FAppModules.Language.GetString('Rainfall.GaugeStatistics'));
        if (FShowGraph) then
        begin
          lGridChecked := FShowGrid;
          if (lGridChecked) then
            DoToggleGrid(Self);
          RainfallGaugeStatsDialog.Repaint;
          RainfallGaugeStatsDialog.RainfallGraph.DoPrint;
          if (lGridChecked) then
            DoToggleGrid(Self);
        end;
      finally
        Printer.Orientation := lPrinterOrientation;
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemInTreeview'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.DoCopyToClipboard;
const OPNAME = 'TRainfallGaugeStatsValidator.DoCopyToClipboard';
begin
  try
    if (RainfallGaugeStatsDialog.GaugeTreeview.Selected <> nil) then
    begin
      if (FShowGrid AND FShowGraph) then
        ShowMessage(FAppModules.Language.GetString('Rainfall.DisplayGraphOrGrid'))
      else
      if (FShowGrid) then
        RainfallGaugeStatsDialog.GaugeGrid.DoCopyToClipboard
      else
      if (FShowGraph) then
        RainfallGaugeStatsDialog.RainfallGraph.DoCopyToClipboard;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemInTreeview'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallGaugeStatsValidator.DoExport';
var
  lGridChecked : Boolean;
  LFilename    : string ;

begin
  try
    if (RainfallGaugeStatsDialog.GaugeTreeview.Selected.Level = 2) then
      LFilename := RainfallGaugeStatsDialog.GaugeTreeview.Selected.Parent.Parent.Text;
    if (RainfallGaugeStatsDialog.GaugeTreeview.Selected.Level = 1) then
      LFilename := RainfallGaugeStatsDialog.GaugeTreeview.Selected.Parent.Text;
    if (RainfallGaugeStatsDialog.GaugeTreeview.Selected.Level = 0) then
      LFilename := RainfallGaugeStatsDialog.GaugeTreeview.Selected.Text;;
    LFilename := StringReplace(LFilename,'*','',[rfReplaceAll, rfIgnoreCase]);
    LFilename := Trim(LFilename);
    if (RainfallGaugeStatsDialog.GaugeTreeview.Selected <> nil) then
    begin
      if (FShowGrid) then
        RainfallGaugeStatsDialog.GaugeGrid.DoExport(LFilename);
      if (FShowGraph) then
      begin
        lGridChecked := FShowGrid;
        if (lGridChecked) then
          DoToggleGrid(Self);
        RainfallGaugeStatsDialog.Repaint;
        RainfallGaugeStatsDialog.RainfallGraph.DoExport(LFilename);
        if (lGridChecked) then
          DoToggleGrid(Self);
      end;
    end
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemInTreeview'));
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.DoGraphTypeClick (Sender : TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.DoGraphTypeClick';
begin
  try
    PopulateGraph(RainfallGaugeStatsDialog.GaugeGrid.Row, TRUE);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsValidator.DoCreateFiles (Sender: TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.DoCreateFiles';
var
  lDirectory    : string;
  lTreeIndex    : integer;
  lStationID    : integer;
  lPatchID      : integer;
  lStation      : IStationData;
  lPatch        : IPatchData;
  lRainfallData : IRainfallModelData;
  lSplit        : IRainfallDataSplit;
  lNode         : TTreeNode;
  LSaveDlg      : TSaveDialog;
  LDirSelected  : boolean;
begin
  try
    with RainfallGaugeStatsDialog do
    begin
      LDirSelected := False;
      lDirectory := GetAppDataLocalDir+'\WRCDATA\'; // ExtractFilePath(ApplicationExeName) + 'wrcdata\';
      LSaveDlg := TSaveDialog.Create(nil);
      LSaveDlg.InitialDir := lDirectory;
      try
        if lDirectory <> '' then
        begin
          lRainfallData := (FAppModules.Model.ModelData as IRainfallModelData);
          for lTreeIndex := 0 to GaugeTreeview.Items.Count - 1 do
          begin
            lNode := GaugeTreeview.Items[lTreeIndex];
            if (lNode.Selected) then
            begin
              if (lNode.Level <= 1) then
              begin
                lStationID := Integer(lNode.Data);
                lStation   := lRainfallData.GetStationDataByID(lStationID);
                if (lStation <> nil) then
                begin
                  if (lNode.Level = 0) then
                    lSplit := lStation.GetSplitWithIndex(0)
                  else
                    lSplit := lStation.GetSplitWithIndex(lNode.Index);

                  if not (LDirSelected) then
                  begin
                    LSaveDlg.FileName :=  lStation.RainfallData.StationNumber;
                    if (LSaveDlg.Execute) then
                      LDirectory := ExtractFilePath(LSaveDlg.FileName);
                  end;
                  LDirSelected := True;

                  lStation.SaveRAWFile(lSplit.HydroStartYear, lSplit.HydroEndYear, lDirectory);
                  lStation.SaveMPFile(lSplit.HydroStartYear, lSplit.HydroEndYear, lDirectory);
                end;
              end
              else
              begin
                lStationID := Integer(lNode.Parent.Data);
                lStation   := lRainfallData.GetStationDataByID(lStationID);
                if (lStation <> nil) then
                begin
                  lPatchID   := Integer(lNode.Data);
                  lPatch     := lStation.GetPatchWithID(lPatchID);
                  if (lPatch <> nil) then
                  begin
                    if not (LDirSelected) then
                    begin
                      LSaveDlg.FileName :=  lPatch.RainfallData.StationNumber;
                      if (LSaveDlg.Execute) then
                        LDirectory := ExtractFilePath(LSaveDlg.FileName);
                    end;
                    LDirSelected := True;
                    lPatch.SaveMPFile(0, 0, lDirectory);
                    lPatch.SavePATFile(0, 0, lDirectory);
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        LSaveDlg.Free;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsValidator.DoCreateSplit (Sender: TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.DoCreateSplit';
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

procedure TRainfallGaugeStatsValidator.DoUpdateSplit(Sender: TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.DoUpdateSplit';
var
  LOldStartYear    : integer;
  LOldEndYear      : integer;
  LNewStartYear    : integer;
  LNewEndYear      : integer;
  LRainfallObj  : IRainfallModelData;
  LStation      : IStationData;
  LSplit        : IRainfallDataSplit;
//  LMessage      : string;
  LForm         : TUpdateSplitForm;
begin
  try
    LStation := nil;
    if (FCurrentStationID <> 0) AND (FCurrentSplitIndex <> -1) then
    begin
      LRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      LStation     := lRainfallObj.GetStationDataByID(FCurrentStationID);
      if (LStation <> nil) then
      begin
        LSplit     := LStation.GetSplitWithIndex(FCurrentSplitIndex);
        LOldStartYear := LSplit.HydroStartYear;
        LOldEndYear   := LSplit.HydroEndYear;
        LNewStartYear := LStation.RainfallData.HydroStartYear;
        LNewEndYear   := LStation.RainfallData.HydroEndYear;

        LForm         := TUpdateSplitForm.CreateWithoutDFM(nil,FAppModules);
        try
          LForm.Initialise;
          LForm.LanguageHasChanged;
          LForm.NewStartYear := LNewStartYear;
          LForm.NewEndYear := LNewEndYear;
          LForm.OldStartYear := LOldStartYear;
          LForm.OldEndYear := LOldEndYear;
          LForm.ShowModal;
          if LForm.ModalResult = mrOK then
          begin
            LRainfallObj.UpdateSplit(lStation.RainfallData.StationID, LOldStartYear,LForm.NewStartYear, LOldEndYear, LForm.NewEndYear);
            PopulateTreeView;
          end;

        finally
          FreeAndNil(LForm);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TRainfallGaugeStatsValidator.DoDeleteSplit(Sender: TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.DoDeleteSplit';
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

procedure TRainfallGaugeStatsValidator.DoToggleTree;
const OPNAME = 'TRainfallGaugeStatsValidator.DoToggleTree';
begin
  try
    FShowTree := NOT FShowTree;
    with RainfallGaugeStatsDialog do
    begin
      PanelTreeView.Visible := FShowTree;
      PanelLeft.Visible     := FShowTree;
      Splitter.Left         := PanelLeft.Left + (PanelLeft.Width + 1);
    end;
    if FShowTree then
      FAppModules.SetMenuItem(CToggleTree, msChecked)
    else
      FAppModules.SetMenuItem(CToggleTree, msUnChecked);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGaugeStatsValidator.OnClickCreateFiles(Sender: TObject);
const OPNAME = 'TRainfallGaugeStatsValidator.OnClickCreateFiles';
begin
  try
    if (RainfallGaugeStatsDialog.GaugeTreeview.Selected <> nil) then
      DoCreateFiles(Sender)
    else
      ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemsInTreeview'));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGaugeStatsValidator.DoIncludeUnreliableCheckBoxClick ( Sender : TObject );
const OPNAME = 'TRainfallGaugeStatsValidator.DoIncludeUnreliableCheckBoxClick';
var
  lRainfallObj : IRainfallModelData;
begin
  try
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lRainfallObj.IncludeUnreliableData := RainfallGaugeStatsDialog.IncludeUnreliableChkBox.Checked;
    PopulateGrid;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.



