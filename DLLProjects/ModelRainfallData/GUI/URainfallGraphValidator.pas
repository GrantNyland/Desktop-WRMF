{******************************************************************************}
{*  UNIT      : Contains the class TRainfallGraphValidator.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/04/28                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallGraphValidator;

interface

uses
  VCL.Buttons,
  VCL.ExtCtrls,
  VCL.Controls,
  VCL.Grids,
  VCL.Forms,
  VCL.ComCtrls,
  Types,
  VCL.Menus,
  VCLTee.Chart,
  VCLTee.Series,
  VCL.Graphics,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  Classes,
  Windows,
  VCL.StdCtrls,
  UConstants,
  UTabsheetManager,
  UAbstractObject,
  UAbstractComponent,
  VoaimsCom_TLB,
  RainfallCom_TLB,
  URainfallGraphMenuItemManager,
  UMenuItemManager,
  UDataComponent,
  URainfallGraphDialog,
  UDataEditComponent,
  UGenericModelLinkClasses,
  UPatchParametersDialog;


type
  TRainfallGraphValidator = class (TAbstractDataDialogValidator)
  private
    FCurrentStationID      : integer;
    FCurrentSplitIndex     : integer;
    FCurrentPatchID        : integer;
    FIsPatchWRC            : Boolean;
    FDailyRatio            : double;
    FGraphRatio            : double;
    FSystemFlag            : boolean;
    FShowGrid              : boolean;
    FShowGraph             : boolean;
    FShowTree              : boolean;
    FHasChanges            : TStringList;
    FHasMetaData           : TStringList;
    FRAWFlags              : TStringList;
    FMenuItemManager       : TRainfallGraphMenuItemManager;
    FAllWeatherEvents      : TStringList;
    FWeatherEvents         : TStringList;
    FWeatherIndex          : integer;
    FNode                  : TTreeNode;
    FHighlightMonths       : TStringList;
    FHighlightYears        : TStringList;
    FChangeListID          : integer;
    FSelectedFlag          : string;
    FChangeDescr           : string;
    FSelectedMonth         : integer;
    FSelectedYear          : integer;
    FQuickFlag             : Boolean;
    FQuickFlagStationID    : integer;
    FScaledDownValuesSaved : boolean;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure RePopulateDataViewer;
    procedure PopulateTreeView;
    function RainfallGraphDialog : TRainfallGraphDialog;
    procedure DoTreeViewChange (Sender : TObject; ANode : TTreeNode);
    procedure DoShowDailyDataGrid (Sender : Tobject);
    procedure DoGridPanelResize(Sender: TObject);
    procedure IncludeUnreliableCheckBoxClick (Sender : TObject);
    procedure DoDrawMonthlyGrid (Sender     : TObject;
                                 ACol, ARow : Longint;
                                 Rect       : TRect;
                                 State      : TGridDrawState);
    procedure DoMonthGridMouseDown (Sender : TObject;
                                    Button : TMouseButton;
                                    Shift  : TShiftState;
                                    X, Y   : integer);
    procedure DoSelectGridCell (Sender        : TObject;
                                ACol          : longint;
                                ARow          : Longint;
                                var CanSelect : Boolean);
    procedure DoHorSplitter2Moved(Sender: TObject);
    procedure DoExitGrid (Sender : TObject);
    procedure DoDrawDailyGrid (Sender     : TObject;
                               aCol, aRow : integer;
                               Rect       : TRect;
                               State      : TGridDrawState );
    procedure DoCloseDailyGrid (Sender : TObject);
    procedure DoSelectGraphType(Sender: TObject);
    procedure DoScrollGraph(Sender: TObject);
    procedure DoPanGraph(Sender: TObject);
    procedure DoZoomGraph(Sender: TObject);

    function HotSpotChangesClicked(X, Y: Integer): boolean;
    function HotSpotMetaDataClicked(X, Y: Integer): boolean;
    procedure DisplayDailyGrid (AShow : boolean);
    procedure PopulateGrid;
    procedure PopulateGraph;
    procedure LoadWeatherEvents;
    procedure PopulateWeatherEvents;
    procedure SetSelectedYearMonth;
    procedure UpdateWeatherEventsIndex (ARow : integer;
                                        ACol : integer);
    function PopulateDailyData (AStationDailyData : TStringList) : boolean;
    function GetPatchOriginalValue  (AStation  : IStationData;
                                     AHydroYearsIndex, AMonthIndex : Integer) : double;
    procedure DisplayGridGraph;
    procedure ResetHasChangesAndMetaData;
    function GetHasChanges (ACol : integer;
                            ARow : integer): boolean;
    procedure SetHasChanges (ACol        : integer;
                             ARow        : Integer;
                             AHasChanges : boolean);
    function GetHasMetaData (ACol : integer;
                             ARow : integer): boolean;
    procedure SetHasMetaData (ACol         : integer;
                              ARow         : Integer;
                              AHasMetaData : boolean);
    procedure ResetWeatherButtons;
    procedure SelectWeatherCell;
    procedure GridCellToCalendarYearMonth (ARow       : integer;
                                           ACol       : integer;
                                           var AYear  : integer;
                                           var AMonth : integer);
    procedure YearMonthToGridCell (AYear    : integer;
                                   AMonth   : integer;
                                   var ARow : integer;
                                   var ACol : integer);
    procedure DoQuickFlag;
    procedure SetQuickFlagOn;
    procedure ResetButtons;
    function isMonthlyScaledDownValuesTableEmpty:boolean;

  public
    function Initialise: boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : String;
                                  AOldValue  : String;
                                  ANewValue  : String): Boolean; override;
    function StudyHasChanged: Boolean;override;
    procedure PopulateDataViewer; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure DoPrint; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoToggleGrid (ASender : TObject);
    procedure DoToggleGraph (ASender : TObject);
    procedure DoToggleTree (ASender : TObject);
    procedure DoCreatePATFiles (ASender : TObject);
    procedure DoHighLightOutliers (ASender : TObject);
    procedure DoSelectRAWFlags(ASender : TObject);
    procedure DoFlagDataBlock (ASender : TObject);
    procedure DoUnFlagDataBlock (ASender : TObject);
    procedure DoFlagSetup (ASender : TObject);
    procedure DoFlagClick (ASender : TObject);
    procedure DoWeatherEvents (Sender : TObject);
    procedure DoFirstWeatherRecord (ASender : TObject);
    procedure DoPatchChangeList (ASender : TObject);

    procedure DoPrevWeatherRecord (ASender : TObject);
    procedure DoNextWeatherRecord (ASender : TObject);
    procedure DoLastWeatherRecord (ASender : TObject);
    procedure DoShowScaledDownValues(ASender : TObject);
    procedure SetQuickFlagOff;
    property MenuItemManager : TRainfallGraphMenuItemManager read FMenuItemManager write FMenuItemManager;

  end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.Dialogs,
  Math,
  VCL.Printers,
  Contnrs,
  VCLTee.TeExport,
  //VCLTee.TeeProcs,
  UDataSetType,
  UMainMenuEventType,
  URainfallFlagDataForm,
  URainfallFlagSetupForm,
  URainfallHighlightForm,
  URainfallCreateFilesForm,
  USelectRAWFlagsForm,
  UErrorHandlingOperations;

{ TRainfallGraphValidator }

procedure TRainfallGraphValidator.CreateMemberObjects;
const OPNAME = 'TRainfallGraphValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FHasChanges       := TStringList.Create;
    FHasMetaData      := TStringList.Create;
    FRAWFlags         := TStringList.Create;
    FAllWeatherEvents := TStringList.Create;
    FWeatherEvents    := TStringList.Create;
    FHighlightMonths  := TStringList.Create;
    FHighlightYears   := TStringList.Create;
    FPanel            := TRainfallGraphDialog.Create ( nil, FAppModules );
    with RainfallGraphDialog do
    begin
      MonthlyGrid.OnDrawCell   := DoDrawMonthlyGrid;
      MonthlyGrid.OnDblClick   := DoShowDailyDataGrid;
      MonthlyGrid.OnClick      := DoShowScaledDownValues;

      MonthlyGrid.OnMouseDown  := DoMonthGridMouseDown;
      MonthlyGrid.OnExit       := DoExitGrid;
      MonthlyGrid.OnSelectCell := DoSelectGridCell;

      DailyGrid.OnDrawCell      := DoDrawDailyGrid;
      BtnCloseDailyGrid.OnClick := DoCloseDailyGrid;
      BarRadioButton.OnClick    := DoSelectGraphType;
      LineRadioButton.OnClick   := DoSelectGraphType;
      GraphScrollBar.OnChange   := DoScrollGraph;
      MonthlyGraph.OnScroll     := DoPanGraph;
      MonthlyGraph.OnZoom       := DoZoomGraph;

      WeatherEventsMenuItem.OnClick   := DoWeatherEvents;
      HorSplitter2.OnMoved            := DoHorSplitter2Moved;
      HorSplitter1.OnMoved            := DoGridPanelResize;
      GraphTreeView.OnChange          := DoTreeViewChange;
      IncludeUnreliableChkBox.OnClick := IncludeUnreliableCheckBoxClick;
    end;
    DisplayDailyGrid(FALSE);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DestroyMemberObjects;
const OPNAME = 'TRainfallGraphValidator.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FHasChanges);
    FreeAndNil(FHasMetaData);
    FreeAndNil(FRAWFlags);
    FreeAndNil(FWeatherEvents);
    FreeAndNil(FAllWeatherEvents);
    FreeAndNil(FHighlightMonths);
    FreeAndNil(FHighlightYears);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGraphValidator.Initialise: Boolean;
const OPNAME = 'TRainfallGraphValidator.Initialise';
begin
  Result := inherited initialise;
  try
    FSystemFlag            := FALSE;
    FGraphRatio            := 0.5;
    FDailyRatio            := 0.3;
    FCurrentPatchID        := 0;
    FCurrentStationID      := 0;
    FCurrentSplitIndex     := -1;
    FChangeListID          := 0;
    FSelectedFlag          := FAppModules.Language.GetString('Rainfall.None');
    FChangeDescr           := '';
    FQuickFlag             := FALSE;
    FIsPatchWRC            := FALSE;
    FShowGrid              := TRUE;
    FShowGraph             := TRUE;
    FShowTree              := TRUE;
    LoadWeatherEvents;
    FWeatherIndex          := -1;
    ResetWeatherButtons;
    Result                 := TRUE;
    FScaledDownValuesSaved := False;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphValidator.RainfallGraphDialog : TRainfallGraphDialog;
const OPNAME = 'TRainfallGraphValidator.RainfallGraphDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TRainfallGraphDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGraphValidator.StudyHasChanged: Boolean;
const OPNAME = 'TRainfallGraphValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallGraphValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                      AFieldName : string;
                                                      AOldValue  : string;
                                                      ANewValue  : string): Boolean;
const OPNAME = 'TRainfallGraphValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    if (AContext <> sdccEdit) OR
       ((AFieldName <> 'MetaDataDateCreated') AND (AFieldName <> 'MetaDataCreatedBy') AND
        (AFieldName <> 'MetaDataComment')) then
      PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGraphValidator.PopulateDataViewer;
const OPNAME = 'TRainfallGraphValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.RePopulateDataViewer;
const OPNAME = 'TRainfallGraphValidator.RePopulateDataViewer';
begin
  try
    FCurrentStationID  := (FAppModules.Model.ModelData as IRainfallModelData).CurrentStationID;
    FCurrentSplitIndex := (FAppModules.Model.ModelData as IRainfallModelData).CurrentSplitIndex;
    FCurrentPatchID    := (FAppModules.Model.ModelData as IRainfallModelData).CurrentPatchID;
    if (FAppModules.Changes.ChangeListWithID(FChangeListID) = nil) then
      FChangeListID    := 0;
    FQuickFlag         := FALSE;

    RainfallGraphDialog.IncludeUnreliableChkBox.Checked :=
      (FAppModules.Model.ModelData as IRainfallModelData).IncludeUnreliableData;
    PopulateTreeView;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.PopulateTreeView;
const OPNAME = 'TRainfallGraphValidator.PopulateTreeView';
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
  lPatch         : IPatchData;
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
    try
      LockWindowUpdate(RainfallGraphDialog.Handle);
      with RainfallGraphDialog do
      begin
        FSystemFlag := TRUE;
        GraphTreeView.Items.Clear;
        lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
        if (lRainfallObj.StationCount = 0) then
        begin
          HeadingLabel.Caption := '';
          IncludeUnreliableChkBox.Enabled := FALSE;
          PanelGraph.Enabled   := FALSE;
          MonthlyGrid.Visible  := FALSE;
          MonthlyGraph.Visible := FALSE;
          ResetButtons;
        end
        else
        begin
          lSelectedNode := nil;
          for LIndex := 0 to lRainfallObj.StationCount - 1 do
          begin
            lStation := lRainfallObj.GetStationDataByIndex(lIndex);
            if (lStation <> nil) then
            begin
              LStationID := lStation.RainfallData.StationID;
              lStationNumber := lStation.RainfallData.StationNumber;
              if (lStation.IsInWR90) then
                lStationNumber := lStationNumber + ' *';
              LCurrentNode := GraphTreeView.Items.AddObject(nil, lStationNumber, TObject(LStationID));
              if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID)) then
                lSelectedNode := LCurrentNode;
              for lSplitIndex := 0 to lStation.SplitCount - 1 do
              begin
                lSplit     := lStation.GetSplitWithIndex(lSplitIndex);
                lSplitName := IntToStr(lSplit.HydroStartYear) + ' - ' + IntToStr(lSplit.HydroEndYear);
                lSplitNode := GraphTreeView.Items.AddChildObject(LCurrentNode, lSplitName, TObject(LStationID));
                if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID) AND
                    (FCurrentSplitIndex <> -1) AND (FCurrentSplitIndex = lSplitIndex)) then
                  lSelectedNode := lSplitNode;
              end;
              for LPatchIndex := 0 to lStation.PatchCount - 1 do
              begin
                lPatch      := lStation.GetPatchWithIndex(LPatchIndex);
                lPatchID    := lPatch.PatchID;
                lSplitNode  := nil;
                lFoundSplit := FALSE;
                if (lPatch.PatchTypeID = 1) then
                begin
                  lSplitNode  := LCurrentNode.Item[0];
                  lFoundSplit := TRUE;
                end
                else
                begin
                  lPatch.GetSourceInfoByStationID(LStationID, lSrcPatchID, lTarget, lStartYear, lEndYear);
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
                  lPatchNode := GraphTreeView.Items.AddChildObject(lSplitNode, lPatch.PatchName, TObject(lPatchID));
                  if ((FCurrentStationID <> 0) AND (FCurrentStationID = LStationID)  AND
                      (FCurrentSplitIndex <> -1) AND (FCurrentSplitIndex = lSplitNode.Index) AND
                      (FCurrentPatchID <> 0) AND (FCurrentPatchID = lPatchID)) then
                    lSelectedNode := lPatchNode;
                end;
              end;
            end;
          end;
          FSystemFlag := FALSE;
          GraphTreeView.FullExpand;
          if (lSelectedNode = nil) AND (GraphTreeView.Items.Count > 0) then
            lSelectedNode := GraphTreeView.Items[0];
          if (lSelectedNode <> nil) then
          begin
            MonthlyGrid.Visible    := TRUE;
            MonthlyGraph.Visible   := TRUE;
            IncludeUnreliableChkBox.Enabled := TRUE;
            PanelGraph.Enabled     := TRUE;
            GraphTreeView.Selected := lSelectedNode;
          end;
        end;
      end;
    finally
      LockWindowUpdate(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.ResetButtons;
const OPNAME = 'TRainfallGraphValidator.ResetButtons';
begin
  try
    if (FMenuItemManager <> nil) then
    begin
      with RainfallGraphDialog do
      begin
        FMenuItemManager.SetToggleGrid((NOT FQuickFlag) AND (GraphTreeView.Selected <> nil));
        FMenuItemManager.SetToggleGraph((NOT FQuickFlag) AND (GraphTreeView.Selected <> nil));
        FMenuItemManager.SetToggleTree((NOT FQuickFlag) AND (GraphTreeView.Selected <> nil));
        FMenuItemManager.SetHighLight((NOT FQuickFlag) AND (GraphTreeView.Selected <> nil));
        FMenuItemManager.SetWeatherEvents((NOT FQuickFlag) AND (GraphTreeView.Selected <> nil));
        FMenuItemManager.SetSelectRAWFlags(NOT FQuickFlag);
        FMenuItemManager.SetFlagData((NOT FQuickFlag) AND (GraphTreeView.Selected <> nil) AND (GraphTreeView.Selected.Level <= 1));
        FMenuItemManager.SetUnFlagData((NOT FQuickFlag) AND (GraphTreeView.Selected <> nil) AND (GraphTreeView.Selected.Level <= 1));
        FMenuItemManager.SetFlagSetup((NOT FQuickFlag) AND (GraphTreeView.Selected <> nil) AND (GraphTreeView.Selected.Level <= 1));
        FMenuItemManager.SetFlagClick((GraphTreeView.Selected <> nil) AND (GraphTreeView.Selected.Level <= 1));

        FMenuItemManager.SetPatchChangeList((GraphTreeView.Selected <> nil) AND (GraphTreeView.Selected.Level >= 2));
        FMenuItemManager.SetCreatePATFiles((NOT FQuickFlag) AND (GraphTreeView.Selected <> nil) AND (GraphTreeView.Selected.Level >= 1));
        FMenuItemManager.ToolBar.FlagClickBtn.Down := FQuickFlag;

        FMenuItemManager.ToolBar.SetPatchChangelistBtn((GraphTreeView.Selected <> nil) AND (GraphTreeView.Selected.Level >= 2));
        ResetWeatherButtons;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.DoTreeViewChange (Sender : TObject;
                                                    ANode  : TTreeNode );
const OPNAME = 'TRainfallGraphValidator.DoTreeViewChange';
var
  lRainfallModelData : IRainfallModelData;
begin
  try
    if ((NOT FSystemFlag) AND Assigned(FAppModules.Model())) AND (Assigned(FAppModules.Model.ModelData())) then
    begin
      lRainfallModelData := (FAppModules.Model.ModelData as IRainfallModelData);
      with RainfallGraphDialog do
      begin
        DisplayDailyGrid(FALSE);
        ResetButtons;
        FNode := ANode;
        if (GraphTreeView.Selected <> nil) then
        begin
          PanelClient.Visible := TRUE;
          PanelGrid.Height   := Trunc((PanelClient.Height - PanelTop.Height - HorSplitter1.Height) *
                                      (1 - FGraphRatio));
          MonthlyGrid.Height := Trunc(PanelTop.Height * (1 - FDailyRatio));
          if (GraphTreeView.Selected.Level = 0) then
          begin
            FCurrentStationID  := Integer(aNode.Data);
            FCurrentSplitIndex := -1;
            FCurrentPatchID    := 0;
            PopulateGrid;
            PopulateGraph;
          end
          else
          if (GraphTreeView.Selected.Level = 1) then
          begin
            FCurrentStationID  := Integer(ANode.Parent.Data);
            FCurrentSplitIndex := ANode.Index;
            FCurrentPatchID    := 0;
            PopulateGrid;
            PopulateGraph;
          end
          else
          if (GraphTreeView.Selected.Level = 2) then
          begin
            FCurrentStationID  := Integer(ANode.Parent.Data);
            FCurrentSplitIndex := ANode.Parent.Index;
            FCurrentPatchID    := Integer(ANode.Data);
            PopulateGrid;
            PopulateGraph;
          end;
        end
        else
          PanelClient.Visible := FALSE;
      end;
      PopulateWeatherEvents;
      SetSelectedYearMonth;
      lRainfallModelData.CurrentPatchID    := FCurrentPatchID;
      lRainfallModelData.CurrentSplitIndex := FCurrentSplitIndex;
      lRainfallModelData.CurrentStationID  := FCurrentStationID;;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.PopulateGrid;
const OPNAME = 'TRainfallGraphValidator.PopulateGrid';
var
  lMonthIdx         : integer;
  lIndex            : integer;
  lHydroCount       : integer;
  lYearlyData       : IYearlyData;
  lFieldIndex       : string;
  lFieldPropData    : TAbstractFieldProperty;
  lFieldPropSign    : TAbstractFieldProperty;
  lParamField       : string;
  lKeyValues        : string;
  lHasChange        : boolean;
  lHasMetaData      : boolean;
  lStation          : IStationData;
  lPatch            : IPatchData;
  lStartYear        : integer;
  lEndYear          : integer;
  lYear             : integer;
  lMonth            : integer;
  lDataCol          : integer;
  lSignCol          : integer;
  lNode             : TTreeNode;
  lRowIndex         : integer;
  lFlag             : string;
  lValue            : double;
  lRainfallData     : IRainfallData;
  lSplit            : IRainfallDataSplit;
  LTempMonth,
  LTempYear         : integer;
  LTempValue        : double;
  LTempSign         : WideString;
begin
//  LValue    := NullFloat;
  try
     if (Assigned(FAppModules.Model())) AND (Assigned(FAppModules.Model.ModelData())) then
    begin
      lRainfallData  := nil;
      lSplit         := nil;
      FHighlightMonths.Clear;
      FHighlightYears.Clear;
      FRAWFlags.Clear;
      FRAWFlags.CommaText := (FAppModules.Model.ModelData as IRainfallModelData).RAWFlags;
      with RainfallGraphDialog do
      begin
        lStation       := nil;
        lPatch         := nil;
        lFieldPropData := nil;
        lFieldPropSign := nil;
        lStartYear     := 0;
        lEndYear       := 0;
        lNode          := GraphTreeView.Selected;
        if (lNode <> nil) then
        begin
          if (lNode.Level >= 0) AND (lNode.Level <= 2) then
          begin
            lStation := (FAppModules.Model.ModelData as IRainfallModelData).
                          GetStationDataByID(FCurrentStationID);
            if (lStation <> nil) then
            begin
              if (lNode.Level = 0) OR (lNode.Level = 1) then
              begin
                lRainfallData  := lStation.RainfallData;
                lFieldPropData := FAppModules.FieldProperties.FieldProperty('MonthlyRAWData');
                lFieldPropSign := FAppModules.FieldProperties.FieldProperty('MonthlyRAWSign');
                if (lNode.Level = 0) then
                begin
                  HeadingLabel.Caption := lStation.RainfallData.StationNumber + ' ' +
                                          FAppModules.Language.GetString('Rainfall.RAWInBrackets') + ' : ' +
                                          FAppModules.Language.GetString('Rainfall.MonthlyRainfall_mm');
                  lStartYear := lRainfallData.HydroStartYear;
                  lEndYear   := lRainfallData.HydroEndYear;
                end
                else
                begin
                  lSplit     := lStation.GetSplitWithIndex(lNode.Index);
                  lStartYear := lSplit.HydroStartYear;
                  lEndYear   := lSplit.HydroEndYear;
                  HeadingLabel.Caption := lStation.RainfallData.StationNumber + ' ' +
                                          FAppModules.Language.GetString('Rainfall.RAWInBrackets') + ' ' +
                                          IntToStr(lStartYear) + '-' + IntToStr(lEndYear) + ' : ' +
                                          FAppModules.Language.GetString('Rainfall.MonthlyRainfall_mm');
                end;
              end
              else
              begin
                lPatch := lStation.GetPatchWithID(FCurrentPatchID);
                if (lPatch <> nil) then
                begin
                  FIsPatchWRC   := lPatch.PatchTypeID = 1;
                  lRainfallData := lPatch.RainfallData;
                  lStartYear    := lRainfallData.HydroStartYear;
                  lEndYear      := lRainfallData.HydroEndYear;
                  if (FIsPatchWRC) then
                  begin
                    HeadingLabel.Caption := lStation.RainfallData.StationNumber + ' ' +
                                            FAppModules.Language.GetString('Rainfall.WRCInBrackets') + ' : ' +
                                            FAppModules.Language.GetString('Rainfall.MonthlyRainfall_mm');
                    lFieldPropData := FAppModules.FieldProperties.FieldProperty('MonthlyWRCData');
                    lFieldPropSign := FAppModules.FieldProperties.FieldProperty('MonthlyWRCSign');
                  end
                  else
                  begin
                    HeadingLabel.Caption := lStation.RainfallData.StationNumber + ' ('+ lPatch.PatchName + '): ' +
                                            FAppModules.Language.GetString('Rainfall.MonthlyRainfall_mm');
                    lFieldPropData := FAppModules.FieldProperties.FieldProperty('MonthlyPatchData');
                    lFieldPropSign := FAppModules.FieldProperties.FieldProperty('MonthlyPatchSign');
                  end
                end;
              end;
            end;
          end;

          if (lRainfallData <> nil) then
          begin
            if (lSplit <> nil) then
            begin
              FHighlightMonths.CommaText := lSplit.HighlightMonths;
              FHighlightYears.CommaText  := lSplit.HighlightYears;
            end
            else
            begin
              FHighlightMonths.CommaText := lRainfallData.HighlightMonths;
              FHighlightYears.CommaText  := lRainfallData.HighlightYears;
            end;
            if ((lStartYear = 0) AND (lEndYear = 0)) then
              lHydroCount := 0
            else
              lHydroCount := lEndYear - lStartYear + 1;
            with RainfallGraphDialog do
            begin
              if MonthlyGrid.RowCount > 1 then
                for lIndex := 1 to MonthlyGrid.RowCount - 1 do
                  MonthlyGrid.Rows[lIndex].Clear;
              MonthlyGrid.FixedCols := 1;
              MonthlyGrid.RowCount  := lHydroCount + 1 + 6;
              ResetHasChangesAndMetaData;
              if (MonthlyGrid.RowCount > 1) then
                MonthlyGrid.FixedRows := 1
              else
                MonthlyGrid.FixedRows := 0;
              lRowIndex  := 0;
              // Calculate totals and counts
              for lIndex := 0 to lRainfallData.HydroYearsCount - 1 do
              begin
                lYearlyData := lRainfallData.GetHydroYearDataByIndex(lIndex);
                if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
                begin
                  lRowIndex := lRowIndex + 1;
                  MonthlyGrid.Objects[0, lRowIndex] := TObject(lYearlyData.Year);
                  MonthlyGrid.Cells[0, lRowIndex]   := lYearlyData.HydroYear;
                  for lMonthIdx := 1 to 12 do
                  begin
                    if (lMonthIdx <= 3) then
                    begin
                      lYear  := lYearlyData.Year;
                      lMonth := lMonthIdx + 9;
                    end
                    else
                    begin
                      lYear  := lYearlyData.Year + 1;
                      lMonth := lMonthIdx - 3;
                    end;
                    lDataCol := lMonthIdx*2 - 1;
                    lSignCol := lMonthIdx*2;

                    lFieldIndex := IntToStr(lYear) + ',' + IntToStr(lMonth);
                    if (lFieldPropData <> nil) then
                      lParamField := lFieldPropData.FieldName;
                    if (lNode.Level <= 1) then
                      lKeyValues  := lStation.GetKeyValues(lParamField, lFieldIndex)
                    else
                      lKeyValues  := lPatch.GetKeyValues(lParamField, lFieldIndex);
                    lHasChange  := (FAppModules.Changes <> nil) AND
                                   (FAppModules.Changes.HasParamChange(lParamField, lKeyValues, IntToStr(lMonth)));
                    SetHasChanges(lDataCol, lRowIndex, lHasChange);
                    lHasMetaData := (FAppModules.MetaData <> nil) AND
                                    (FAppModules.MetaData.FindMetaData(lParamField, lKeyValues, IntToStr(lMonth)) <> nil);
                    SetHasMetaData(lDataCol, lRowIndex, lHasMetaData);

                    if (lFieldPropSign <> nil) then
                      lParamField := lFieldPropSign.FieldName;
                    if (lNode.Level <= 1) then
                      lKeyValues  := lStation.GetKeyValues(lParamField, lFieldIndex)
                    else
                      lKeyValues  := lPatch.GetKeyValues(lParamField, lFieldIndex);
                    lHasChange  := (FAppModules.Changes <> nil) AND
                                   (FAppModules.Changes.HasParamChange(lParamField, lKeyValues, IntToStr(lMonth)));
                    SetHasChanges(lSignCol, lRowIndex, lHasChange);
                    lHasMetaData := (FAppModules.MetaData <> nil) AND
                                    (FAppModules.MetaData.FindMetaData(lParamField, lKeyValues, IntToStr(lMonth)) <> nil);
                    SetHasMetaData(lSignCol, lRowIndex, lHasMetaData);
                    lFlag  := lYearlyData.MonthlyPatchSign[lMonthIdx];

                    LTempMonth := lMonthIdx;
                    LTempYear  := LYearlyData.Year;

                    if (LTempMonth > 0 )and (LTempMonth <= 3) then
                    begin
                      LTempMonth := LTempMonth + 9;
                      LTempYear  := LTempYear;
                    end
                    else
                    begin
                      LTempMonth := LTempMonth - 3;
                      LTempYear := LTempYear +1;
                    end;

                    if (lFlag = '*')then
                    begin
                      LStation.RainfallData.GetBaseDataForYearAndMonth(LTempYear,LTempMonth,LTempValue,LTempSign);
                      if(LTempValue <> NullFloat)then
                      begin
                        if (lPatch.Get_PatchScaledDownStatus(FCurrentStationID,FCurrentPatchID,LTempYear,LTempMonth))then
                          lValue := lYearlyData.MonthlyRainfall[lMonthIdx]
                        else
                          lValue := LTempValue;
                      end
                      else
                        lValue := lYearlyData.MonthlyRainfall[lMonthIdx];
                    end
                    else
                      lValue := lYearlyData.MonthlyRainfall[lMonthIdx];

                    MonthlyGrid.Cells[lSignCol, lRowIndex] := lFlag;
                    if (lValue = NullFloat) then
                    begin
                      MonthlyGrid.Cells[lDataCol, lRowIndex] := '';
                    end
                    else
                    begin
                      MonthlyGrid.Cells[lDataCol, lRowIndex] := Format('%6.1f', [lValue]);
                    end;
                  end;
                  if (lYearlyData.HasMissingData) then
                    MonthlyGrid.Cells[25, lRowIndex] := Format('%6.1f', [lYearlyData.Total]) + '+'
                  else
                  begin
                    MonthlyGrid.Cells[25, lRowIndex] := Format('%6.1f', [lYearlyData.Total]);
                  end;
                end;
              end;

              MonthlyGrid.Cells[ 0, MonthlyGrid.RowCount - 6 ] := FAppModules.Language.GetString('Rainfall.Avg');
              MonthlyGrid.Cells[ 0, MonthlyGrid.RowCount - 5 ] := FAppModules.Language.GetString('Rainfall.StdDev');
              MonthlyGrid.Cells[ 0, MonthlyGrid.RowCount - 4 ] := FAppModules.Language.GetString('Rainfall.CV');
              MonthlyGrid.Cells[ 0, MonthlyGrid.RowCount - 3 ] := FAppModules.Language.GetString('Rainfall.Max');
              MonthlyGrid.Cells[ 0, MonthlyGrid.RowCount - 2 ] := FAppModules.Language.GetString('Rainfall.Min');
              MonthlyGrid.Cells[ 0, MonthlyGrid.RowCount - 1 ] := FAppModules.Language.GetString('Rainfall.Cnt');

              if (lSplit <> nil) then
              begin
                if (IncludeUnreliableChkBox.Checked) then
                begin
                  for lMonthIdx := 1 to 12 do
                  begin
                    lDataCol := lMonthIdx*2 - 1;
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-6] := Format('%6.1f', [lSplit.MonthMAP[lMonthIdx]]);
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-5] := Format('%6.1f', [lSplit.MonthStdDev[lMonthIdx]]);
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-4] := Format('%6.1f', [lSplit.MonthCV[lMonthIdx]]);
                    if (lSplit.MonthMax[lMonthIdx] <> NullFloat) then
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-3] := Format('%6.1f', [lSplit.MonthMax[lMonthIdx]])
                    else
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-3] := '';
                    if (lSplit.MonthMin[lMonthIdx] <> NullFloat) then
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-2] := Format('%6.1f', [lSplit.MonthMin[lMonthIdx]])
                    else
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-2] := '';
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-1] := Format('%6d', [lSplit.MonthCount[lMonthIdx]]);
                  end;
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-6] := Format('%6.1f', [lSplit.MAP]);
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-5] := Format('%6.1f', [lSplit.StdDeviation]);
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-4] := Format('%6.1f', [lSplit.CV]);
                  if (lSplit.YearMin <> NullFloat) then
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-3] := Format('%6.1f', [lSplit.YearMax])
                  else
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-3] := '';
                  if (lSplit.YearMax <> NullFloat) then
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-2] := Format('%6.1f', [lSplit.YearMin])
                  else
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-2] := '';
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-1] := Format('%3d', [lSplit.YearCount]);
                end
                else
                begin
                  for lMonthIdx := 1 to 12 do
                  begin
                    lDataCol := lMonthIdx*2 - 1;
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-6] := Format('%6.1f', [lSplit.XMonthMAP[lMonthIdx]]);
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-5] := Format('%6.1f', [lSplit.XMonthStdDev[lMonthIdx]]);
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-4] := Format('%6.1f', [lSplit.XMonthCV[lMonthIdx]]);
                    if (lSplit.XMonthMax[lMonthIdx] <> NullFloat) then
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-3] := Format('%6.1f', [lSplit.XMonthMax[lMonthIdx]])
                    else
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-3] := '';
                    if (lSplit.XMonthMin[lMonthIdx] <> NullFloat) then
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-2] := Format('%6.1f', [lSplit.XMonthMin[lMonthIdx]])
                    else
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-2] := '';
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-1] := Format('%6d', [lSplit.XMonthCount[lMonthIdx]]);
                  end;
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-6] := Format('%6.1f', [lSplit.XMAP]);
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-5] := Format('%6.1f', [lSplit.XStdDeviation]);
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-4] := Format('%6.1f', [lSplit.XCV]);
                  if (lSplit.XYearMin <> NullFloat) then
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-3] := Format('%6.1f', [lSplit.XYearMax])
                  else
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-3] := '';
                  if (lSplit.XYearMax <> NullFloat) then
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-2] := Format('%6.1f', [lSplit.XYearMin])
                  else
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-2] := '';
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-1] := Format('%3d', [lSplit.XYearCount]);
                end;
              end
              else
              begin {FRainfall}
                if (IncludeUnreliableChkBox.Checked) then
                begin
                  for lMonthIdx := 1 to 12 do
                  begin
                    lDataCol := lMonthIdx*2 - 1;
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-6] := Format('%6.1f', [lRainfallData.MonthMAP[lMonthIdx]]);
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-5] := Format('%6.1f', [lRainfallData.MonthStdDev[lMonthIdx]]);
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-4] := Format('%6.1f', [lRainfallData.MonthCV[lMonthIdx]]);
                    if (lRainfallData.MonthMax[lMonthIdx] <> NullFloat) then
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-3] := Format('%6.1f', [lRainfallData.MonthMax[lMonthIdx]])
                    else
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-3] := '';
                    if (lRainfallData.MonthMin[lMonthIdx] <> NullFloat) then
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-2] := Format('%6.1f', [lRainfallData.MonthMin[lMonthIdx]])
                    else
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-2] := '';
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-1] := Format('%6d', [lRainfallData.MonthCount[lMonthIdx]]);
                  end;
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-6] := Format('%6.1f', [lRainfallData.MAP]);
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-5] := Format('%6.1f', [lRainfallData.StdDeviation]);
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-4] := Format('%6.1f', [lRainfallData.CV]);
                  if (lRainfallData.YearMin <> NullFloat) then
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-3] := Format('%6.1f', [lRainfallData.YearMin])
                  else
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-3] := '';
                  if (lRainfallData.YearMax <> NullFloat) then
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-2] := Format('%6.1f', [lRainfallData.YearMax])
                  else
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-2] := '';
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-1] := Format('%3d', [lRainfallData.YearCount]);
                end
                else
                begin
                  for lMonthIdx := 1 to 12 do
                  begin
                    lDataCol := lMonthIdx*2 - 1;
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-6] := Format('%6.1f', [lRainfallData.XMonthMAP[lMonthIdx]]);
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-5] := Format('%6.1f', [lRainfallData.XMonthStdDev[lMonthIdx]]);
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-4] := Format('%6.1f', [lRainfallData.XMonthCV[lMonthIdx]]);
                    if (lRainfallData.XMonthMax[lMonthIdx] <> NullFloat) then
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-3] := Format('%6.1f', [lRainfallData.XMonthMax[lMonthIdx]])
                    else
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-3] := '';
                    if (lRainfallData.XMonthMin[lMonthIdx] <> NullFloat) then
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-2] := Format('%6.1f', [lRainfallData.XMonthMin[lMonthIdx]])
                    else
                      MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-2] := '';
                    MonthlyGrid.Cells[lDataCol, MonthlyGrid.RowCount-1] := Format('%6d', [lRainfallData.XMonthCount[lMonthIdx]]);
                  end;
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-6] := Format('%6.1f', [lRainfallData.XMAP]);
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-5] := Format('%6.1f', [lRainfallData.XStdDeviation]);
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-4] := Format('%6.1f', [lRainfallData.XCV]);
                  if (lRainfallData.XYearMin <> NullFloat) then
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-3] := Format('%6.1f', [lRainfallData.XYearMin])
                  else
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-3] := '';
                  if (lRainfallData.XYearMax <> NullFloat) then
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-2] := Format('%6.1f', [lRainfallData.XYearMax])
                  else
                    MonthlyGrid.Cells[25, MonthlyGrid.RowCount-2] := '';
                  MonthlyGrid.Cells[25, MonthlyGrid.RowCount-1] := Format('%3d', [lRainfallData.XYearCount]);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.PopulateGraph;
const OPNAME = 'TRainfallGraphValidator.PopulateGraph';
var
  lIndex            : integer;
  lMonth            : integer;
  lMonthLabel       : string;
  lMonthValue       : double;
  lYearlyData       : IYearlyData;
  lTitle            : string;
  lStation          : IStationData;
  lPatch            : IPatchData;
  lRainfallData     : IRainfallData;
  lNode             : TTreeNode;
  lStartYear        : integer;
  lEndYear          : integer;
  lSplit            : IRainfallDataSplit;
begin
  try
    if (Assigned(FAppModules.Model())) AND (Assigned(FAppModules.Model.ModelData())) then
    begin
      with RainfallGraphDialog do
      begin
        lStation       := nil;
        lPatch         := nil;
        lRainfallData  := nil;
        lNode          := GraphTreeView.Selected;
        lStartYear     := 0;
        lEndYear       := 0;
        if (lNode.Level >= 0) AND (lNode.Level <= 2) then
        begin

          lStation := (FAppModules.Model.ModelData as IRainfallModelData).
                        GetStationDataByID(FCurrentStationID);
          if (lStation <> nil) then
          begin
            if (lNode.Level = 0) OR (lNode.Level = 1) then
            begin
              lRainfallData  := lStation.RainfallData;
              if (lNode.Level = 0) then
              begin
                lStartYear := lRainfallData.HydroStartYear;
                lEndYear   := lRainfallData.HydroEndYear;
              end
              else
              begin
                lSplit     := lStation.GetSplitWithIndex(lNode.Index);
                lStartYear := lSplit.HydroStartYear;
                lEndYear   := lSplit.HydroEndYear;
              end;
            end
            else
            begin
              lPatch := lStation.GetPatchWithID(FCurrentPatchID);
              if (lPatch <> nil) then
              begin
                lRainfallData := lPatch.RainfallData;
                lStartYear := lRainfallData.HydroStartYear;
                lEndYear   := lRainfallData.HydroEndYear;
              end;
            end;
          end;
        end;
        LineSeries.Clear;
        BarSeries.Clear;
        if (lRainfallData <> nil) then
        begin
          lTitle := FAppModules.Language.GetString('Rainfall.MonthlyRainfall');
          if (FCurrentStationID <> 0) then
          begin
            lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FCurrentStationID);
            if (lStation <> nil) then
            begin
              lTitle := lStation.RainfallData.StationNumber + ' : ' +
                        FAppModules.Language.GetString('Rainfall.MonthlyRainfall_mm');
              if (FCurrentPatchID <> 0)then
              begin
                lPatch := lStation.GetPatchWithID(FCurrentPatchID);
                if (lPatch <> nil) then
                  lTitle := lStation.RainfallData.StationNumber + ' (' + lPatch.PatchName + ') : ' +
                            FAppModules.Language.GetString('Rainfall.MonthlyRainfall_mm');
              end;
            end;
            MonthlyGraph.Title.Text.Clear;
            MonthlyGraph.Title.Text.Add(lTitle);
            for LIndex := 0 to lRainfallData.HydroYearsCount - 1 do
            begin
              lYearlyData := lRainfallData.GetHydroYearDataByIndex(lIndex);
              if ((lYearlyData.Year >= lStartYear) AND (lYearlyData.Year <= lEndYear)) then
              begin
                for LMonth := 1 to 12 do
                begin
                  if (lMonth <= 3) then
                    lMonthLabel := IntToStr(lYearlyData.Year) + '/' + Format('%2.2d', [lMonth + 9])
                  else
                    lMonthLabel := IntToStr(lYearlyData.Year + 1) + '/' + Format('%2.2d', [lMonth - 3]);
                  lMonthValue := lYearlyData.MonthlyRainfall[lMonth];
                  if (lMonthValue <> NullFloat) then
                  begin
                    LineSeries.AddY(lMonthValue, lMonthLabel, clTeeColor);
                    BarSeries.AddY(lMonthValue, lMonthLabel, clTeeColor);
                  end
                  else
                  begin
                    LineSeries.AddY(0, lMonthLabel, clTeeColor);
                    BarSeries.AddY(0, lMonthLabel, clTeeColor);
                  end;
                end;
              end;
            end;
            GraphScrollBar.Min := 1;
            GraphScrollBar.Max := MonthlyGraph.NumPages;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.LoadWeatherEvents;
const OPNAME = 'TRainfallGraphValidator.LoadWeatherEvents';
begin
  try
    FAllWeatherEvents.Clear;
    FAllWeatherEvents.CommaText := FAppModules.WeatherEvents.Data.FindWeatherEvents(0, 0, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.PopulateWeatherEvents;
const OPNAME = 'TRainfallGraphValidator.PopulateWeatherEvents';
var
  lStation          : IStationData;
  lPatch            : IPatchData;
  lRainfallData     : IRainfallData;
  lStartDate        : string;
  lEndDate          : string;
  lDateStr          : string;
  lIndex            : integer;
  lDateIdx          : integer;
begin
  try
    if (NOT FSystemFlag) AND (Assigned(FAppModules.Model())) AND (Assigned(FAppModules.Model.ModelData())) then
    begin
      FWeatherEvents.Clear;
      FWeatherIndex := -1;
      with RainfallGraphDialog do
      begin
        lStation       := nil;
        lPatch         := nil;
        lRainfallData  := nil;
        if (GraphTreeView.Selected <> nil) then
        begin
          lStation := (FAppModules.Model.ModelData as IRainfallModelData).
                        GetStationDataByID(FCurrentStationID);
          if (lStation <> nil) then
          begin
            if (GraphTreeView.Selected.Level < 2) then
              lRainfallData  := lStation.RainfallData
            else
            begin
              lPatch := lStation.GetPatchWithID(FCurrentPatchID);
              if (lPatch <> nil) then
                lRainfallData := lPatch.RainfallData;
            end;
          end;
        end;
        if (lRainfallData <> nil) then
        begin
          lStartDate := FormatDateTime('yyyymm', lRainfallData.HydroStartDate);
          lEndDate   := FormatDateTime('yyyymm', lRainfallData.HydroEndDate);
        end;
        if (FAllWeatherEvents.Count > 0) then
        begin
          lIndex := 0;
          while (lIndex <= FAllWeatherEvents.Count - 1) do
          begin
            lDateStr := Copy(FAllWeatherEvents.Strings[lIndex], 1, 6);
            if (lDateStr > lEndDate) then
              Break
            else
            if (lDateStr >= lStartDate) then
            begin
              lDateIdx := FWeatherEvents.IndexOf(lDateStr);
              if (lDateIdx < 0) then
                FWeatherEvents.Add(lDateStr);
              lIndex := lIndex + 1;
            end
            else
              lIndex := lIndex + 1;
          end;
        end;
      end;
      ResetWeatherButtons;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.UpdateWeatherEventsIndex (ARow : integer;
                                                            ACol : integer);
const OPNAME = 'TRainfallGraphValidator.UpdateWeatherEventsIndex';
var
  lYear    : integer;
  lMonth   : integer;
  lDateStr : string;
  lIndex   : integer;
begin
  try
    if (NOT FSystemFlag) then
    begin
      FWeatherIndex := -1;
      GridCellToCalendarYearMonth(ARow, ACol, lYear, lMonth);
      if (lYear > 0) AND (lMonth > 0) AND (FWeatherEvents.Count > 0) then
      begin
        lDateStr := IntToStr(lYear) + Format('%2.2d', [lMonth]);
        lIndex   := FWeatherEvents.IndexOf(lDateStr);
        if (lIndex >= 0) then
          FWeatherIndex := lIndex
        else
        begin
          lIndex := 0;
          while ((lIndex < FWeatherEvents.Count) AND (lDateStr > FWeatherEvents.Strings[lIndex])) do
            lIndex := lIndex + 1;
          if (lIndex <> 0) AND (lIndex <> FWeatherEvents.Count) then
            FWeatherIndex := lIndex;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.ResetWeatherButtons;
const OPNAME = 'TRainfallGraphValidator.ResetWeatherButtons';
begin
  try
    if (FMenuItemManager <> nil) then
    begin
      FMenuItemManager.ToolBar.SetFirstRecord((NOT FQuickFlag) AND
                                              (FWeatherEvents.Count > 0) AND
                                              (FWeatherIndex <> 0));
      FMenuItemManager.ToolBar.SetPrevRecord((NOT FQuickFlag) AND
                                             (FWeatherEvents.Count > 0) AND
                                             (FWeatherIndex > 0));
      FMenuItemManager.ToolBar.SetNextRecord((NOT FQuickFlag) AND
                                             (FWeatherEvents.Count > 0) AND
                                             (FWeatherIndex > -1) AND
                                             (FWeatherIndex < FWeatherEvents.Count - 1));
      FMenuItemManager.ToolBar.SetLastRecord((NOT FQuickFlag) AND
                                             (FWeatherEvents.Count > 0) AND
                                             (FWeatherIndex <> FWeatherEvents.Count - 1));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.SelectWeatherCell;
const OPNAME = 'TRainfallGraphValidator.SelectWeatherCell';
var
  lTempStr    : string;
  lYearStr    : string;
  lMonthStr   : string;
  lYear       : word;
  lMonth      : word;
  lRow        : integer;
  lCol        : integer;
begin
  try
    lTempStr  := FWeatherEvents.Strings[FWeatherIndex];
    lYearStr  := Copy(lTempStr, 1, 4);
    if(Pos('/',lTempStr)>0 ) or (Pos('-',lTempStr)>0 ) or (Pos('\',lTempStr)>0 ) then
      lMonthSTr := Copy(lTempStr, 6, 2)
    else
      lMonthSTr := Copy(lTempStr, 5, 2);

    lYear     := StrToInt(lYearStr);
    lMonth    := StrToInt(lMonthStr);
    with RainfallGraphDialog do
    begin
      YearMonthToGridCell(lYear, lMonth, lRow, lCol);
      if (lRow > 0) AND (lRow < MonthlyGrid.RowCount - 6) AND
         (lCol > 0) AND (lCol < MonthlyGrid.ColCount - 1) then
      begin
        FSystemFlag := TRUE;
        MonthlyGrid.Row := lRow;
        MonthlyGrid.Col := lCol;
        FSystemFlag := FALSE;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoFirstWeatherRecord (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoFirstWeatherRecord';
begin
  try
    FWeatherIndex := 0;
    SelectWeatherCell;
    ResetWeatherButtons;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoNextWeatherRecord (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoNextWeatherRecord';
begin
  try
    FWeatherIndex := FWeatherIndex + 1;
    SelectWeatherCell;
    ResetWeatherButtons;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoPrevWeatherRecord (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoPrevWeatherRecord';
begin
  try
    FWeatherIndex := FWeatherIndex - 1;
    SelectWeatherCell;
    ResetWeatherButtons;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoLastWeatherRecord (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoLastWeatherRecord';
begin
  try
    FWeatherIndex := FWeatherEvents.Count - 1;
    SelectWeatherCell;
    ResetWeatherButtons;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoGridPanelResize(Sender: TObject);
const OPNAME = 'TRainfallGraphValidator.DoGridPanelResize';
begin
  try
    with RainfallGraphDialog do
    begin
      if (PanelGrid.Height < (MonthlyGrid.RowHeights[0] + DailyGrid.RowHeights[0])) then
        PanelGrid.Height := MonthlyGrid.RowHeights[0] + DailyGrid.RowHeights[0];
      if (DailyGrid.Visible AND (DailyGrid.Top > PanelGrid.Height)) then
        MonthlyGrid.Height := Trunc(PanelGrid.Height * (1 - FDailyRatio));
      if ((NOT FSystemFlag) AND (MonthlyGraph.Height < PanelClient.Height) AND
          FShowGrid AND FShowGraph) then
      begin
        FGraphRatio := PanelGraph.Height /
                       (PanelClient.Height - PanelTop.Height - HorSplitter1.Height);
        DoHorSplitter2Moved(Self);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.IncludeUnreliableCheckBoxClick (Sender : TObject);
const OPNAME = 'TRainfallGraphValidator.IncludeUnreliableCheckBoxClick';
var
  lRainfallObj : IRainfallModelData;
begin
  try
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lRainfallObj.IncludeUnreliableData := RainfallGraphDialog.IncludeUnreliableChkBox.Checked;
    PopulateGrid;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DisplayGridGraph;
const OPNAME = 'TRainfallGraphValidator.DisplayGridGraph';
begin
  try
    FSystemFlag := TRUE;
    with RainfallGraphDialog do
    begin
      BarRadioButton.Enabled  := FShowGraph;
      LineRadioButton.Enabled := FShowGraph;
      PanelGrid.Visible    := FShowGrid;
      HorSplitter1.Visible := FShowGrid AND FShowGraph;
      PanelGraph.Visible   := FShowGraph;
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
          PanelGrid.Align := alTop;
          PanelGrid.Height := Trunc((PanelClient.Height - PanelTop.Height - HorSplitter1.Height) *
                                     (1 - FGraphRatio));
        end;
      end;
      if (PanelGraph.Visible) then
        PanelGraph.Align := alClient;
      PanelGrid.Top    := 25;
      HorSplitter1.Top := PanelGrid.Top + PanelGrid.Height;
      PanelGraph.Top   := HorSplitter1.Top + HorSplitter1.Height;
    end;
    FSystemFlag := FALSE;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoToggleGrid(ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoToggleGrid';
begin
  try
    FShowGrid := NOT FShowGrid;
    DisplayGridGraph;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.DoToggleGraph(ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoToggleGraph';
begin
  try
    FShowGraph := NOT FShowGraph;
    DisplayGridGraph;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.DoToggleTree (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoToggleTree';
begin
  try
    FShowTree := NOT FShowTree;
    with RainfallGraphDialog do
    begin
      GraphTreeView.Visible := FShowTree;
      VerticalSplitter.Left := GraphTreeView.Left + (GraphTreeView.Width + 1);
    end;
    if FShowTree then
      FAppModules.SetMenuItem(CToggleTree, msChecked)
    else
      FAppModules.SetMenuItem(CToggleTree, msUnChecked);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoDrawMonthlyGrid (Sender     : TObject;
                                                     ACol, ARow : Longint;
                                                     Rect       : TRect;
                                                     State      : TGridDrawState);
const OPNAME = 'TRainfallGraphValidator.DoDrawMonthlyGrid';
var
  lGrid          : TStringGrid;
  lFlag          : string;
  lValueStr      : string;
  lFieldPropSign : TAbstractFieldProperty;
  lOutlier       : boolean;
  lMonth         : integer;
  lYear          : integer;
  lDateStr       : string;
  LValue         : double;
  LSign          : WideString;
  LStation       : IStationData;
  lRainfallObj   : IRainfallModelData;
begin
  try
    with RainfallGraphDialog do
    begin
      if ((FAppModules.Model <> nil) AND (FAppModules.Model.ModelData <> nil) AND (Sender <> nil) AND
          (FCurrentStationID <> 0) AND (GraphTreeView <> nil) AND (GraphTreeView.Selected <> nil)) then
      begin
        lGrid := TStringGrid(Sender);
        if ((ACol >= lGrid.FixedCols) AND
            (ARow >= lGrid.FixedRows) AND
            (ACol < MonthlyGrid.ColCount - 1) AND
            (ARow < MonthlyGrid.RowCount - 6)) then
        begin
          lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
          lStation     := lRainfallObj.GetStationDataByID(FCurrentStationID);
          GridCellToCalendarYearMonth(ARow, ACol, lYear, lMonth);
          LStation.RainfallData.GetBaseDataForYearAndMonth(lYear,lMonth,LValue,LSign);
          if ((ACol mod 2) = 0) then
            lFlag := Trim(lGrid.Cells[ACol, ARow])
          else
            lFlag := Trim(lGrid.Cells[ACol+1, ARow]);
          if ((ACol mod 2) = 0) then
            lValueStr := Trim(lGrid.Cells[ACol-1, ARow])
          else
            lValueStr := Trim(lGrid.Cells[ACol, ARow]);
          lOutlier := FALSE;
          GridCellToCalendarYearMonth(ARow, ACol, lYear, lMonth);
          if (lYear > 0) AND (lMonth > 0) then
          begin
            lDateStr := Format('%4d%2d', [lYear, lMonth]);
            lOutlier := FHighlightMonths.IndexOf(lDateStr) >= 0;
          end;
          if ((lFlag <> '')) then
          begin
            if (lGrid.Canvas.Brush.Color = clHighlight) then
              lGrid.Canvas.Font.Color := clWhite{clBlack}
            else
            if (lFlag = '[') then
              lGrid.Canvas.Brush.Color := clRainBlue
            else
            if (lFlag = '*')and(LValue <> NullFloat) then
            begin
              if StrToFloatDef(lValueStr,-1) > 0 then
                lGrid.Canvas.Font.Color := clRed;
              lGrid.Canvas.Brush.Color := clWhite;
            end;

            if (GraphTreeView.Selected.Level = 2) then
            begin
              if (FIsPatchWRC) then
              begin
                lFieldPropSign := FAppModules.FieldProperties.FieldProperty('MonthlyWRCSign');
                if (Pos(lFlag, lFieldPropSign.FieldAcceptedValues) > 0) OR (FRAWFlags.IndexOf(lFlag) >= 0) then
                begin
                  lGrid.Canvas.Brush.Color := clRainYellow;
                end
                else
                if (lOutlier) then
                  lGrid.Canvas.Brush.Color := clRainPink;
              end
              else {PatchR}
              begin
                lGrid.Canvas.Brush.Color :=clRainYellow;
              end;
            end
            else
            if (GraphTreeView.Selected.Level < 2) then
            begin
              if (FRAWFlags.IndexOf(lFlag) >= 0) then
                lGrid.Canvas.Brush.Color := clRainGreen
              else
              if (lOutlier) then
                lGrid.Canvas.Brush.Color := clRainPink;
            end;
          end
          else
          if (lOutlier) then
            lGrid.Canvas.Brush.Color := clRainPink;
          lGrid.Canvas.TextRect(Rect, Rect.Left, Rect.Top, lGrid.Cells[ACol, ARow]);
          if (GetHasChanges(ACol, ARow)) then
          begin
            lGrid.Canvas.Brush.Color := clLime;
            lGrid.Canvas.Pen.Color   := clBlack;
            lGrid.Canvas.Polygon([Point(Rect.Left,     Rect.Top),
                                  Point(Rect.Left + 7, Rect.Top),
                                  Point(Rect.Left,     Rect.Top + 7)]);
          end;
          if (GetHasMetaData(ACol, ARow)) then
          begin
            lGrid.Canvas.Brush.Color := clAqua;
            lGrid.Canvas.Pen.Color   := clBlack;
            lGrid.Canvas.Brush.Color := clAqua;
            lGrid.Canvas.Pen.Color   := clBlack;
            lGrid.Canvas.Polygon([Point(Rect.Left,     Rect.Bottom),
                                  Point(Rect.Left + 7, Rect.Bottom),
                                  Point(Rect.Left,     Rect.Bottom - 7)]);
          end;
        end
        else
        begin
          if ((ARow >= MonthlyGrid.RowCount - 6) OR (ACol = MonthlyGrid.ColCount - 1) ) then
          begin
            lGrid.Canvas.Brush.Color := clBtnFace;
            lGrid.Canvas.TextRect(Rect, Rect.Left, Rect.Top, lGrid.Cells[ACol, ARow]);
            if ((ACol = MonthlyGrid.ColCount - 1) AND (ARow > 0) AND (ARow < MonthlyGrid.RowCount - 6)) then
            begin
              lYear     := Integer(RainfallGraphDialog.MonthlyGrid.Objects[0, ARow]);
              lDateStr  := IntToStr(lYear);
              lValueStr := Trim(lGrid.Cells[ACol, ARow]);
              if (Length(lValueStr) > 0) AND (lValueStr[Length(lValueStr)] <> '+') then
              begin
                if (FHighlightYears.IndexOf(lDateStr) >= 0) then
                  lGrid.Canvas.Brush.Color := clRainPink;
                lGrid.Canvas.TextRect(Rect, Rect.Left, Rect.Top, lGrid.Cells[ACol, ARow]);
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoScrollGraph(Sender: TObject);
const OPNAME = 'TRainfallGraphValidator.DoScrollGraph';
begin
  try
    if (NOT FSystemFlag) then
    begin
      FSystemFlag := TRUE;
      with RainfallGraphDialog do
      begin
        MonthlyGraph.Page := GraphScrollBar.Position;
      end;
      FSystemFlag := FALSE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoPanGraph(Sender: TObject);
const OPNAME = 'TRainfallGraphValidator.DoPanGraph';
begin
  try
    if (NOT FSystemFlag) then
    begin
      FSystemFlag := TRUE;
      with RainfallGraphDialog do
      begin
        GraphScrollBar.Position := MonthlyGraph.Page;
      end;
      FSystemFlag := FALSE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoZoomGraph(Sender: TObject);
const OPNAME = 'TRainfallGraphValidator.DoZoomGraph';
begin
  try
    if (NOT FSystemFlag) then
    begin
      FSystemFlag := TRUE;
      with RainfallGraphDialog do
      begin
        GraphScrollBar.Max      := MonthlyGraph.NumPages;
        GraphScrollBar.Position := MonthlyGraph.Page;
      end;
      FSystemFlag := FALSE;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoPrint;
const OPNAME = 'TRainfallGraphValidator.DoPrint';
var
  lPrinterOrientation : TPrinterOrientation;
  lGridChecked        : Boolean;
  lIndex              : integer;
  lPage               : integer;
  lTitle              : string;
  lStation            : IStationData;
  lPatch              : IPatchData;
begin
  try
    with RainfallGraphDialog do
    begin
      if (GraphTreeView.Selected <> nil) then
      begin
        lPrinterOrientation := Printer.Orientation;
        try
          Printer.Orientation := poLandscape;
          if (FCurrentStationID <> 0) then
          begin
            lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FCurrentStationID);
            if ((lStation <> nil) AND (FCurrentPatchID <> 0))then
            begin
              lPatch := lStation.GetPatchWithID(FCurrentPatchID);
              if (lPatch <> nil) then
                lTitle := lStation.RainfallData.StationNumber + ' (' + lPatch.PatchName + ') : ' +
                          FAppModules.Language.GetString('Rainfall.MonthlyRainfall_mm')
              else
                lTitle := lStation.RainfallData.StationNumber + ' : ' +
                          FAppModules.Language.GetString('Rainfall.MonthlyRainfall_mm');
            end
            else
              lTitle := lStation.RainfallData.StationNumber + ' ' +
                        FAppModules.Language.GetString('Rainfall.RAWInBrackets') + ' : ' +
                        FAppModules.Language.GetString('Rainfall.MonthlyRainfall_mm');
          end;
          if (FShowGrid) then
          begin
            MonthlyGrid.DoPrint(lTitle);
          end;
          if (FShowGraph) then
          begin
            lGridChecked := FShowGrid;
            if (lGridChecked) then
              DoToggleGrid(Self);
            Repaint;
            lPage := MonthlyGraph.Page;
            MonthlyGraph.Title.Text.Add(lTitle);
            for lIndex := 1 to MonthlyGraph.NumPages do
            begin
              MonthlyGraph.Page := lIndex;
              MonthlyGraph.DoPrint;
            end;
            MonthlyGraph.Title.Text.Clear;
            MonthlyGraph.Page    := lPage;
            if (lGridChecked) then
              DoToggleGrid(Self);
          end;
        finally
          Printer.Orientation := lPrinterOrientation;
        end;
      end
      else
        ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemInTreeview'));
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGraphValidator.DoCopyToClipboard;
const OPNAME = 'TRainfallGraphValidator.DoCopyToClipboard';
begin
  try
    with RainfallGraphDialog do
    begin
      if (GraphTreeView.Selected <> nil) then
      begin
        if (FShowGrid AND FShowGraph) then
          ShowMessage(FAppModules.Language.GetString('Rainfall.DisplayGraphOrGrid'))
        else
        if (FShowGrid) then
          MonthlyGrid.DoCopyToClipboard
        else
        if (FShowGraph) then
          MonthlyGraph.DoCopyToClipboard;
      end
      else
        ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemInTreeview'));
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGraphValidator.DoExport(AFileName: string = '');
const OPNAME = 'TRainfallGraphValidator.DoExport';
var
  lGridChecked : Boolean;
begin
  try
    with RainfallGraphDialog do
    begin
      if (GraphTreeView.Selected <> nil) then
      begin
        if (FShowGrid) then
          MonthlyGrid.DoExport(AFileName);
        if (FShowGraph) then
        begin
          lGridChecked := FShowGrid;
          if (lGridChecked) then
            DoToggleGrid(Self);
          Repaint;
          TeeExport(Self.FPanelOwner, VCLTee.TeeProcs.TCustomTeePanel(MonthlyGraph));
          if (lGridChecked) then
            DoToggleGrid(Self);
        end;
      end
      else
        ShowMessage(FAppModules.Language.GetString('Rainfall.SelectItemInTreeview'));
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRainfallGraphValidator.DoSelectGraphType(Sender: TObject);
const OPNAME = 'TRainfallGraphValidator.DoSelectGraphType';
begin
  try
    with RainfallGraphDialog do
    begin
      if (LineRadioButton.Checked) then
      begin
        BarSeries.Active  := FALSE;
        LineSeries.Active := TRUE;
      end
      else
      begin
        LineSeries.Active := FALSE;
        BarSeries.Active  := TRUE;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.ResetHasChangesAndMetaData;
const OPNAME = 'TRainfallGraphValidator.ResetHasChangesAndMetaData';
var
  lIndex : integer;
begin
  try
    FHasChanges.Clear;
    FHasMetaData.Clear;
    with RainfallGraphDialog do
    begin
      for lIndex := 0 to (MonthlyGrid.RowCount * MonthlyGrid.ColCount) -1 do
      begin
        FHasChanges.Add('');
        FHasMetaData.Add('');
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.SetHasChanges (ACol        : integer;
                                                 ARow        : Integer;
                                                 AHasChanges : boolean);
const OPNAME = 'TRainfallGraphValidator.SetHasChanges';
var
  lIndex : integer;
begin
  try
    lIndex := (ARow * RainfallGraphDialog.MonthlyGrid.ColCount) + ACol;
    if (AHasChanges) then
      FHasChanges[lIndex] := 'Y'
    else
      FHasChanges[lIndex] := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGraphValidator.GetHasChanges (ACol : integer;
                                                ARow : integer): boolean;
const OPNAME = 'TRainfallGraphValidator.GetHasChanges';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    lIndex := (ARow * RainfallGraphDialog.MonthlyGrid.ColCount) + ACol;
    Result := (LIndex < FHasChanges.Count) and (FHasChanges[lIndex] = 'Y');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.SetHasMetaData (ACol         : integer;
                                                  ARow         : Integer;
                                                  AHasMetaData : boolean);
const OPNAME = 'TRainfallGraphValidator.SetHasMetaData';
var
  lIndex : integer;
begin
  try
    lIndex := (ARow * RainfallGraphDialog.MonthlyGrid.ColCount) + ACol;
    if (AHasMetaData) then
      FHasMetaData[lIndex] := 'Y'
    else
      FHasMetaData[lIndex] := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGraphValidator.GetHasMetaData (ACol : integer;
                                                 ARow : integer): boolean;
const OPNAME = 'TRainfallGraphValidator.GetHasMetaData';
var
  lIndex : integer;
begin
  Result := FALSE;
  try
    lIndex := (ARow * RainfallGraphDialog.MonthlyGrid.ColCount) + ACol;
    Result := (LIndex < FHasMetaData.Count) AND (FHasMetaData[lIndex] = 'Y');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.YearMonthToGridCell (AYear    : integer;
                                                       AMonth   : integer;
                                                       var ARow : integer;
                                                       var ACol : integer);
const OPNAME = 'TRainfallGraphValidator.YearMonthToGridCell';
var
  l1stGrdYear : integer;
begin
  try
    ARow := 0;
    ACol := 0;
    with RainfallGraphDialog do
    begin
      if (Copy(MonthlyGrid.Cells[0, 1], 5, 1) = '/') then
      begin
        l1stGrdYear := StrToInt(Copy(MonthlyGrid.Cells[0, 1], 1, 4));
        if (AMonth < 10) then
        begin
          ARow := AYear - l1stGrdYear;
          ACol := (AMonth + 3 - 1) * 2 + 1;
        end
        else
        begin
          ARow := AYear - l1stGrdYear + 1;
          ACol := (AMonth - 9 - 1) * 2 + 1;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.GridCellToCalendarYearMonth (ARow       : integer;
                                                               ACol       : integer;
                                                               var AYear  : integer;
                                                               var AMonth : integer);
const OPNAME = 'TRainfallGraphValidator.GridCellToCalendarYearMonth';
begin
  try
   AYear  := 0;
   AMonth := 0;
    with RainfallGraphDialog do
    begin
      try
        if Length(Trim(Copy(MonthlyGrid.Cells[0, ARow], 1, 4)))> 0 then
        begin
          AYear  := StrToIntDef(Copy(MonthlyGrid.Cells[0, ARow], 1, 4),NullInteger);
          if(AYear = NullInteger) then
            AYear  := 0
          else
          begin
            if (ACol <= 6) then
              AMonth := ((ACol+1) div 2) + 9
            else
            begin
              AMonth := ((ACol+1) div 2) - 3;
              AYear  := AYear + 1;
            end;
          end;
        end;
      except
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.DoShowDailyDataGrid (Sender : Tobject);
const OPNAME = 'TRainfallGraphValidator.DoShowDailyDataGrid';
var
  lYear             : integer;
  lMonth            : integer;
  lStationDailyData : TStringList;
  lPatch            : IPatchData;
  lStation          : IStationData;
begin
  try
    with RainfallGraphDialog do
    begin
      DisplayDailyGrid(FALSE);
      if ((MonthlyGrid.Col > 0) AND (MonthlyGrid.Col < MonthlyGrid.ColCount - 1) AND
          (MonthlyGrid.Row > 0) AND (MonthlyGrid.Row < MonthlyGrid.RowCount - 6)) then
      begin
        if (FCurrentStationID <> 0) then
        begin
          lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FCurrentStationID);
          if ((lStation <> nil) AND (FCurrentPatchID <> 0))then
          begin
            lPatch := lStation.GetPatchWithID(FCurrentPatchID);
            if ((lPatch <> nil) AND (lPatch.PatchTypeID = 1)) then
            begin
              GridCellToCalendarYearMonth(MonthlyGrid.Row, MonthlyGrid.Col, lYear, lMonth);
              if (lYear > 0) AND (lMonth > 0) then
              begin
                lStationDailyData := TStringList.Create;
                try
                  lStationDailyData.CommaText :=
                    (FAppModules.Model.ModelData as IRainfallModelData).
                      GetDailyDataByMonthAndYear(lStation.RainfallData.StationID, lPatch.PatchID, lMonth, lYear);
                  if ((lStationDailyData.Count > 0) AND (lStationDailyData.Count <> 0)) then
                    PopulateDailyData(lStationDailyData)
                  else
                    ShowMessage(FAppModules.Language.GetString('Rainfall.NoDailyDataAvailable'));
                finally
                  FreeAndNil(lStationDailyData);
                end;
              end;
            end
            else
              ShowMessage(FAppModules.Language.GetString('Rainfall.NoDailyDataAvailable'));
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphValidator.PopulateDailyData (AStationDailyData : TStringList) : boolean;
const OPNAME = 'TRainfallGraphValidator.PopulateDailyData';
var
  lPos,
  lPatchPos,
  lCount,
  lColIndex,
  lCol,
  lIndex : integer;
  lPatch,
  lValue : string;
begin
  Result := False;
  try
    with RainfallGraphDialog do
    begin
      DisplayDailyGrid(TRUE);
      if DailyGrid.RowCount > 1 then
        for lIndex := 0 to DailyGrid.RowCount - 1 do
          DailyGrid.Rows [ lIndex ].Clear;

      DailyGrid.RowCount := aStationDailyData.Count + 1;
      if (DailyGrid.RowCount > 1) then
        DailyGrid.FixedRows := 1;
      DailyGrid.ColCount := 65;
      DailyGrid.Cells[0, 0] := FAppModules.Language.GetString('Rainfall.Station');
      DailyGrid.Cells[1, 0] := 'Y';
      DailyGrid.Cells[2, 0] := 'M';
      DailyGrid.ColWidths[0] := 63;
      DailyGrid.ColWidths[1] := 31;
      for lIndex := 3 to DailyGrid.ColCount -1 do
        if ((lIndex mod 2) = 0) then
          DailyGrid.Cells[lIndex, 0] := IntToStr((lIndex div 2) - 1)
        else
          DailyGrid.ColWidths[lIndex] := 10;
      for lIndex := 1 to aStationDailyData.Count do
      begin
        lPatchPos := 17;
        lPos := 18;
        lCount := 4;
        DailyGrid.Cells [ 0, lIndex ] := Copy ( aStationDailyData [ lIndex -1 ], 1, 10 );
        DailyGrid.Cells [ 1, lIndex ] := Copy ( aStationDailyData [ lIndex -1 ], 11, 4 );
        DailyGrid.Cells [ 2, lIndex ] := Copy ( aStationDailyData [ lIndex -1 ], 15, 2 ) ;
        lCol := 4;
        for lColIndex := 2 to DailyGrid.ColCount do
        begin
          lPatch := Copy ( aStationDailyData [ lIndex -1 ], lPatchPos, 1 );
          lValue := Copy ( aStationDailyData [ lIndex -1 ], lPos, lCount );
          DailyGrid.Cells [ lColIndex * 2 - 1, lIndex ] := lPatch;
          DailyGrid.Cells [ lCol, lIndex ] := lValue;
          lPatchPos := lPatchPos + lCount + 1;
          lPos := lPatchPos + 1;
          lCol := lCol + 2;
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoHorSplitter2Moved(Sender: TObject);
const OPNAME = 'TRainfallGraphValidator.DoHorSplitter2Moved';
begin
  try
    with RainfallGraphDialog do
    begin
      if (MonthlyGrid.Height < MonthlyGrid.RowHeights[0]) then
        MonthlyGrid.Height := MonthlyGrid.RowHeights[0];
      FDailyRatio := DailyGrid.Height / DailyGrid.Parent.Height;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoMonthGridMouseDown (Sender : TObject;
                                                        Button : TMouseButton;
                                                        Shift  : TShiftState;
                                                        X, Y   : integer);
const OPNAME = 'TRainfallGraphValidator.DoMonthGridMouseDown';
var
  lRow : integer;
  lCol : integer;
begin
  try
    with RainfallGraphDialog do
    begin
      if (Button = mbRight) then
      begin
        MonthlyGrid.MouseToCell(X, Y, lCol, lRow);
        if ((GraphTreeView.Selected.Level = 0) AND
            (FCurrentStationID <> 0) AND
            (lCol > 0) AND (lCol < MonthlyGrid.ColCount - 1) AND
            (lRow > 0) AND (lRow < MonthlyGrid.RowCount - 6)) then
          MonthlyGrid.PopupMenu := DataPopupMenu
        else
          MonthlyGrid.PopupMenu := nil;
      end
      else
      if (Button = mbLeft) then
      begin
        MonthlyGrid.MouseToCell(X, Y, lCol, lRow);
        if (FQuickFlag) AND
           (lCol > 0) AND (lCol < MonthlyGrid.ColCount - 1) AND
           (lRow > 0) AND (lRow < MonthlyGrid.RowCount - 6) AND
           ((lCol mod 2) = 0) then
          DoQuickFlag
        else
        if HotSpotChangesClicked(X, Y) then
        begin
          if (GetHasChanges(MonthlyGrid.Col, MonthlyGrid.Row)) then
            FAppModules.Model.ProcessEvent(CmeChangeParameter,nil);
        end
        else
        if HotSpotMetaDataClicked(X, Y) then
        begin
          if (GetHasMetaData(MonthlyGrid.Col, MonthlyGrid.Row)) then
            FAppModules.Model.ProcessEvent(CmeMetaData,nil);
        end;
        GridCellToCalendarYearMonth (MonthlyGrid.Row,MonthlyGrid.Col,FSelectedYear,FSelectedMonth);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGraphValidator.HotSpotChangesClicked(X, Y: Integer): boolean;
const OPNAME = 'TRainfallGraphValidator.HotSpotChangesClicked';
var
  LCurrentCell : TRect;
  lSize        : integer;
begin
  Result := FALSE;
  try
    with RainfallGraphDialog do
    begin
      LCurrentCell := MonthlyGrid.CellRect(MonthlyGrid.Col, MonthlyGrid.Row);
      lSize := 7;
      Result := (X >= 0) AND
                (Y >= 0) AND
                (X >= LCurrentCell.Left) AND
                (X <= (LCurrentCell.Left + lSize)) AND
                (Y >= LCurrentCell.Top) AND
                (Y <= LCurrentCell.Top + lSize);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGraphValidator.HotSpotMetaDataClicked(X, Y: Integer): boolean;
const OPNAME = 'TRainfallGraphValidator.HotSpotMetaDataClicked';
var
  LCurrentCell : TRect;
  lSize        : integer;
begin
  Result := False;
  try
    with RainfallGraphDialog do
    begin
      LCurrentCell := MonthlyGrid.CellRect(MonthlyGrid.Col, MonthlyGrid.Row);
      lSize := 7;
      Result := (X >= 0) and
                (Y >= 0) and
                (X >= LCurrentCell.Left) AND
                (X <= (LCurrentCell.Left + lSize)) AND
                (Y <= LCurrentCell.Bottom) AND
                (Y >= LCurrentCell.Bottom - lSize);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.DoExitGrid (Sender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoExitGrid';
begin
  try
    FAppModules.Changes.SetParameterChanges(FALSE);
    if (FQuickFlag) then
      SetQuickFlagOff;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoSelectGridCell (Sender        : TObject;
                                                    ACol          : longint;
                                                    ARow          : Longint;
                                                    var CanSelect : Boolean);

const OPNAME = 'TRainfallGraphValidator.DoSelectGridCell';
begin
  try
    with RainfallGraphDialog do
    begin
      if ((ACol >= MonthlyGrid.FixedCols) AND
          (ARow >= MonthlyGrid.FixedRows) AND
          (ACol < MonthlyGrid.ColCount - 1) AND
          (ARow < MonthlyGrid.RowCount - 6)) then
      begin
        FMenuItemManager.ToolBar.SetParameterChange(True);
        FAppModules.Changes.SetParameterChanges(TRUE);
        UpdateWeatherEventsIndex(ARow, ACol);
        ResetWeatherButtons;

      end
      else
      begin
        FMenuItemManager.ToolBar.SetParameterChange(False);
        FAppModules.Changes.SetParameterChanges(FALSE);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DisplayDailyGrid (AShow : boolean);
const OPNAME = 'TRainfallGraphValidator.DisplayDailyGrid';
begin
  try
    with RainfallGraphDialog do
    begin
      if (AShow) then
      begin
        if (NOT DailyGrid.Visible) then
        begin
          MonthlyGrid.Align    := alTop;
          MonthlyGrid.Height   := Trunc(PanelGrid.Height * (1 - FDailyRatio));
          HorSplitter2.Visible := TRUE;
          HorSplitter2.Top     := MonthlyGrid.Top + MonthlyGrid.Height + 5;
          DailyGrid.Visible    := TRUE;

          PanelCloseDailyGrid.Visible := True;
          PanelCloseDailyGrid.Height := DailyGrid.Height;
        end;
      end
      else
      begin
        if (DailyGrid.Visible) then
        begin
          DailyGrid.Visible    := FALSE;
          HorSplitter2.Visible := FALSE;
          MonthlyGrid.Align    := alClient;

          PanelCloseDailyGrid.Visible := False;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoWeatherEvents (Sender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoWeatherEvents';
var
  lYear        : integer;
  lMonth       : integer;
  lStartDate   : TDateTime;
  lEndDate     : TDateTime;
  lRow         : integer;
  lCol         : integer;
begin
  try
    with RainfallGraphDialog do
    begin
      lRow := MonthlyGrid.Row;
      lCol := MonthlyGrid.Col;
      if ((lCol > 0) AND (lCol < MonthlyGrid.ColCount - 1) AND
          (lRow > 0) AND (lRow < MonthlyGrid.RowCount - 6)) then
      begin
        GridCellToCalendarYearMonth(MonthlyGrid.Row, MonthlyGrid.Col, lYear, lMonth);
        if (lYear > 0) AND (lMonth > 0) then
        begin
          lStartDate := EncodeDate(lYear, lMonth, 1);
          lEndDate   := EncodeDate(lYear, lMonth, DaysInMonth(lYear, lMonth));
          if (Assigned(FAppModules.WeatherEvents())) then
            FAppModules.WeatherEvents.ShowWeatherEvents(lStartDate, lEndDate, '');
        end;    
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallGraphValidator.DoDrawDailyGrid (Sender     : TObject;
                                                   aCol, aRow : integer;
                                                   Rect       : TRect;
                                                   State      : TGridDrawState);
const OPNAME = 'TRainfallGraphValidator.DoDrawDailyGrid';
var
  lDailyGrid : TStringGrid;
begin
  try
    lDailyGrid := TStringGrid(Sender);
    if ((ARow <> 0) AND (ACol > 2) AND (ACol mod 2 <> 0)) then
    begin
      lDailyGrid.Canvas.Font.Color := clRed;
      if (lDailyGrid.Canvas.Brush.Color = clHighlight) then
        lDailyGrid.Canvas.Font.Color := clBlack;
      lDailyGrid.Canvas.Brush.Color := clRainYellow;
      lDailyGrid.Canvas.TextRect(Rect, Rect.Left, Rect.Top, lDailyGrid.Cells[ACol, ARow]);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoCloseDailyGrid ( Sender : TObject );
const OPNAME = 'TRainfallGraphValidator.DoCloseDailyGrid';
begin
  try
    DisplayDailyGrid ( False );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoCreatePATFiles (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoCreatePATFiles';
var
  lForm : TRainfallCreateFilesForm;
begin
  try
    lForm := TRainfallCreateFilesForm.CreateWithoutDFM(nil, FAppModules);
    try
      lForm.Initialise;
      lForm.LanguageHasChanged;
      with RainfallGraphDialog do
      begin
        lForm.SetData(FCurrentStationID, FCurrentSplitIndex, FCurrentPatchID);
        lForm.ShowModal;
      end;
    finally
      FreeAndNil(lForm);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoHighLightOutliers (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoHighLightOutliers';
var
  lForm        : TRainfallHighlightForm;
  lRainfallObj : IRainfallModelData;
  lIndex       : integer;
  lStation     : IStationData;
begin
  try
    lForm := TRainfallHighlightForm.CreateWithoutDFM(nil, FAppModules);
    try
      lForm.Initialise;
      lForm.LanguageHasChanged;
      lForm.ShowModal;
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      for lIndex := 0 to lRainfallObj.StationCount - 1 do
      begin
        lStation := lRainfallObj.GetStationDataByIndex(lIndex);
        lStation.RepopulateHighlights;
      end;
      PopulateGrid;
    finally
      FreeAndNil(lForm);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoSelectRAWFlags(ASender: TObject);
const OPNAME = 'TRainfallGraphValidator.DoSelectRAWFlags';
var
  lFieldProperty : TAbstractFieldProperty;
  lForm          : TSelectRawFlagsForm;
  lStation       : IStationData;
begin
  try
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWSign');
    if (lFieldProperty <> nil) then
    begin
      lForm := TSelectRawFlagsForm.CreateWithoutDFM(nil, FAppModules);
      try
        lForm.Initialise;
        lForm.LanguageHasChanged;
        lForm.SetData(lFieldProperty.FieldName, FCurrentStationID, FCurrentPatchID);
        lForm.ShowModal;
        lStation := (FAppModules.Model.ModelData as IRainfallModelData).
                      GetStationDataByID(FCurrentStationID);
        if (lStation <> nil) then
        begin
          lStation.LoadMonthlyData;
          lStation := nil;
          PopulateGrid;
        end;
      finally
        FreeAndNil(lForm);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoFlagDataBlock (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoFlagDataBlock';
var
  lFieldProperty : TAbstractFieldProperty;
  lStation       : IStationData;
  lPatch         : IPatchData;
  lForm          : TRainfallFlagDataForm;
  lChangeList    : IChangeList;
begin
  try
    if (FAppModules.Changes = nil) then
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseCreateChangeList'))
    else
    begin
      if (FAppModules.Changes.ChangeLists.Count = 0) then
      begin
        lChangeList := FAppModules.Changes.DoCreateNewChangeList;
        lChangeList.ChangeListName := FAppModules.Language.GetString('Rainfall.FlagsList');
      end;
      lFieldProperty := nil;
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FCurrentStationID);
      if (lStation <> nil) then
      begin
        if (FCurrentPatchID = 0) then
          lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWSign')
        else
        begin
          lPatch := lStation.GetPatchWithID(FCurrentPatchID);
          if (lPatch <> nil) then
          begin
            if (lPatch.PatchTypeID = 1) then
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyWRCSign')
            else
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyPatchSign');
          end;
        end;
      end;

      if (lFieldProperty <> nil) then
      begin
        lForm := TRainfallFlagDataForm.CreateWithoutDFM(nil, FAppModules);
        try
          lForm.Initialise;
          lForm.LanguageHasChanged;
          lForm.SetData(TRUE, lFieldProperty.FieldName, FCurrentStationID, FCurrentPatchID);
          lForm.ShowModal;
          lPatch   := nil;
          lStation.LoadMonthlyData;
          lStation := nil;
          PopulateGrid;
          PopulateGraph;
        finally
          FreeAndNil(lForm);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoUnFlagDataBlock (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoUnFlagDataBlock';
var
  lFieldProperty : TAbstractFieldProperty;
  lStation       : IStationData;
  lPatch         : IPatchData;
  lForm          : TRainfallFlagDataForm;
begin
  try
    if (FAppModules.Changes = nil) then
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseCreateChangeList'))
    else
    if (FAppModules.Changes.ChangeLists.Count = 0) then
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseCreateChangeList'))
    else
    begin
      lFieldProperty := nil;
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FCurrentStationID);
      if (lStation <> nil) then
      begin
        if (FCurrentPatchID = 0) then
          lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWSign')
        else
        begin
          lPatch := lStation.GetPatchWithID(FCurrentPatchID);
          if (lPatch <> nil) then
          begin
            if (lPatch.PatchTypeID = 1) then
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyWRCSign')
            else
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyPatchSign');
          end;
        end;
      end;

      if (lFieldProperty <> nil) then
      begin
        lForm := TRainfallFlagDataForm.CreateWithoutDFM(nil, FAppModules);
        try
          lForm.Initialise;
          lForm.LanguageHasChanged;
          lForm.SetData(FALSE, lFieldProperty.FieldName, FCurrentStationID, FCurrentPatchID);
          lForm.ShowModal;
          lPatch   := nil;
          lStation.LoadMonthlyData;
          lStation := nil;
          PopulateGrid;
          PopulateGraph;
        finally
          FreeAndNil(lForm);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphValidator.ProcessParameterChangeEvent: Boolean;
const OPNAME = 'TRainfallGraphValidator.ProcessParameterChangeEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lStation       : IStationData;
  lPatch         : IPatchData;
  lYear          : integer;
  lMonth         : integer;
begin
  Result := FALSE;
  try
    with RainfallGraphDialog do
    begin
      if (FPanel.Visible AND MonthlyGrid.Focused AND
          (MonthlyGrid.Col > 0) AND (MonthlyGrid.Col < MonthlyGrid.ColCount - 1) AND
          (MonthlyGrid.Row > 0) AND (MonthlyGrid.Row < MonthlyGrid.RowCount - 6)) then
      begin
        if (FCurrentStationID <> 0) then
        begin
          GridCellToCalendarYearMonth(MonthlyGrid.Row, MonthlyGrid.Col, lYear, lMonth);
          if (lYear > 0) AND (lMonth > 0) then
          begin
            lFieldIndex := IntToStr(lYear) + ',' + IntToStr(lMonth);
            lFieldProperty := nil;
            lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FCurrentStationID);
            if (lStation <> nil) then
            begin
              if (FCurrentPatchID = 0) then
              begin
                if ((MonthlyGrid.Col mod 2) = 0) then
                  lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWSign')
                else
                  lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWData');
                lKeyValues     := lStation.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              end
              else
              begin
                lPatch := lStation.GetPatchWithID(FCurrentPatchID);
                if (lPatch <> nil) then
                begin
                  if (lPatch.PatchTypeID = 1) then
                  begin
                    if ((MonthlyGrid.Col mod 2) = 0) then
                      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyWRCSign')
                    else
                      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyWRCData');
                  end
                  else
                  begin
                    if ((MonthlyGrid.Col mod 2) = 0) then
                      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyPatchSign')
                    else
                      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyPatchData');
                  end;
                  lKeyValues     := lPatch.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
                end;
              end;
            end;
            lFieldIndex := IntToStr(lMonth);
            if (lFieldProperty <> nil) then
            begin
              FAppModules.Changes.ShowParameterChanges
                (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              lPatch   := nil;
              lStation.LoadMonthlyData;
              lStation := nil;
              PopulateGrid;
              PopulateGraph;
              FAppModules.Changes.SetParameterChanges(TRUE);
              Result := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallGraphValidator.ProcessMetaDataEvent: Boolean;
const OPNAME = 'TRainfallGraphValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lStation       : IStationData;
  lPatch         : IPatchData;
  lYear          : integer;
  lMonth         : integer;
begin
  Result := FALSE;
  try
    with RainfallGraphDialog do
    begin
      if (FPanel.Visible AND MonthlyGrid.Focused AND
          (MonthlyGrid.Col > 0) AND (MonthlyGrid.Col < MonthlyGrid.ColCount - 1) AND
          (MonthlyGrid.Row > 0) AND (MonthlyGrid.Row < MonthlyGrid.RowCount - 6)) then
      begin
        if (FCurrentStationID <> 0) then
        begin
          GridCellToCalendarYearMonth(MonthlyGrid.Row, MonthlyGrid.Col, lYear, lMonth);
          if (lYear > 0) AND (lMonth > 0) then
          begin
            lFieldIndex := IntToStr(lYear) + ',' + IntToStr(lMonth);
            lFieldProperty := nil;
            lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FCurrentStationID);
            if (lStation <> nil) then
            begin
              if (FCurrentPatchID = 0) then
              begin
                if ((MonthlyGrid.Col mod 2) = 0) then
                  lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWSign')
                else
                  lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWData');
                lKeyValues     := lStation.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              end
              else
              begin
                lPatch := lStation.GetPatchWithID(FCurrentPatchID);
                if (lPatch <> nil) then
                begin
                  if (lPatch.PatchTypeID = 1) then
                  begin
                    if ((MonthlyGrid.Col mod 2) = 0) then
                      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyWRCSign')
                    else
                      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyWRCData');
                  end
                  else
                  begin
                    if ((MonthlyGrid.Col mod 2) = 0) then
                      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyPatchSign')
                    else
                      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyPatchData');
                  end;
                  lKeyValues     := lPatch.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
                end;
              end;
            end;
            lFieldIndex := IntToStr(lMonth);
            if (lFieldProperty <> nil) then
            begin
              FAppModules.MetaData.ShowMetaData
                (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              PopulateGrid;
              Result := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallGraphValidator.DoFlagSetup (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoFlagSetup';
var
  lFieldProperty : TAbstractFieldProperty;
  lForm          : TRainfallFlagSetupForm;
  lChangeList    : IChangeList;
begin
  try
    if (FAppModules.Changes = nil) then
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseCreateChangeList'))
    else
    begin
      if (FAppModules.Changes.ChangeLists.Count = 0) then
      begin
        lChangeList := FAppModules.Changes.DoCreateNewChangeList;
        lChangeList.ChangeListName := FAppModules.Language.GetString('Rainfall.FlagsList');
      end;

      lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWSign');
      if (lFieldProperty <> nil) then
      begin
        lForm := TRainfallFlagSetupForm.CreateWithoutDFM(nil, FAppModules);
        try
          lForm.Initialise;
          lForm.LanguageHasChanged;
          lForm.SetData(FChangeListID, FSelectedFlag, lFieldProperty.FieldName);
          lForm.ShowModal;
          if (lForm.ModalResult = mrOk) then
          begin
            FChangeListID := lForm.ChangeListID;
            FSelectedFlag := lForm.SelectedFlag;
            FChangeDescr  := lForm.ChangeDescr;
          end;
        finally
          FreeAndNil(lForm);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoFlagClick (ASender : TObject);
const OPNAME = 'TRainfallGraphValidator.DoFlagClick';
begin
  try
    if (FQuickFlag) then
      SetQuickFlagOff
    else
    begin
      if (FChangeListID = 0) OR (FSelectedFlag = '') then
        DoFlagSetup(Self);
      if (FSelectedFlag = 'None') then
        FSelectedFlag := '';
      if (FChangeListID = 0) then
      begin
        ShowMessage(FAppModules.Language.GetString('Rainfall.SelectChangeListAndFlag'));
        FMenuItemManager.ToolBar.FlagClickBtn.Down := FALSE;
      end;
      SetQuickFlagOn;
    end;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoQuickFlag;
const OPNAME = 'TRainfallGraphValidator.DoQuickFlag';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lStation       : IStationData;
  lPatch         : IPatchData;
  lYear          : integer;
  lMonth         : integer;
  lChangeList    : IChangeList;
  lMessage       : string;
  lParamChange   : IParameterChange;
begin
  try
    with RainfallGraphDialog do
    begin
      if (FCurrentStationID <> 0) then
      begin
        GridCellToCalendarYearMonth(MonthlyGrid.Row, MonthlyGrid.Col, lYear, lMonth);
        if (lYear > 0) AND (lMonth > 0) then
        begin
          lFieldIndex := IntToStr(lYear) + ',' + IntToStr(lMonth);
          lFieldProperty := nil;
          lStation := (FAppModules.Model.ModelData as IRainfallModelData).GetStationDataByID(FCurrentStationID);
          if (lStation <> nil) then
          begin
            if (FCurrentPatchID = 0) then
            begin
              lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWSign');
              lKeyValues := lStation.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
            end
            else
            begin
              lPatch := lStation.GetPatchWithID(FCurrentPatchID);
              if (lPatch <> nil) then
              begin
                if (lPatch.PatchTypeID = 1) then
                  lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyWRCSign')
                else
                  lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyPatchSign');
                lKeyValues := lPatch.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              end;
            end;
            lFieldIndex := IntToStr(lMonth);
            lChangeList := FAppModules.Changes.ChangeListWithID(FChangeListID);
            if (lChangeList <> nil) then
            begin
              lParamChange := lChangeList.FindParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              if (lParamChange <> nil) then
              begin
                if (lParamChange.Change <> FSelectedFlag) then
                begin
                  lMessage := lParamChange.Change + ' ' + FAppModules.Language.GetString('Rainfall.FlagAlreadyExists');
                  lParamChange := nil;
                  ShowMessage(lMessage);
                end
                else
                begin
                  lParamChange := nil;
                  lChangeList.DeleteParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex);
                  SetHasChanges(MonthlyGrid.Col, MonthlyGrid.Row, FALSE);
                  MonthlyGrid.Cells[MonthlyGrid.Col, MonthlyGrid.Row] := '';
                  MonthlyGrid.Cells[MonthlyGrid.Col-1, MonthlyGrid.Row] := MonthlyGrid.Cells[MonthlyGrid.Col-1, MonthlyGrid.Row];
                end;
              end
              else
              begin
                lChangeList.CreateNewParamChange(lFieldProperty.FieldName, lKeyValues, lFieldIndex,
                                                 'Y', FSelectedFlag,FChangeDescr,False);
                SetHasChanges(MonthlyGrid.Col, MonthlyGrid.Row, TRUE);
                MonthlyGrid.Cells[MonthlyGrid.Col, MonthlyGrid.Row] := FSelectedFlag;
                MonthlyGrid.Cells[MonthlyGrid.Col-1, MonthlyGrid.Row] := MonthlyGrid.Cells[MonthlyGrid.Col-1, MonthlyGrid.Row];
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.SetQuickFlagOn;
const OPNAME = 'TRainfallGraphValidator.SetQuickFlagOn';
begin
  try
    FQuickFlag := TRUE;
    FQuickFlagStationID  := FCurrentStationID;
    ResetButtons;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.SetQuickFlagOff;
const OPNAME = 'TRainfallGraphValidator.SetQuickFlagOff';
var
  lStation : IStationData;
begin
  try
    if (FQuickFlag) then
    begin
      FQuickFlag := FALSE;
      lStation := (FAppModules.Model.ModelData as IRainfallModelData).
                    GetStationDataByID(FQuickFlagStationID);
      if (lStation <> nil) then
      begin
        lStation.LoadMonthlyData;
        lStation := nil;
      end;
      if (FQuickFlagStationID = FCurrentStationID) then
        PopulateGrid;
      ResetButtons;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.SetSelectedYearMonth;
const OPNAME = 'TRainfallGraphValidator.SetSelectedYearMonth';
var
  LRow : integer;
  LCol : integer;
begin
  try
    YearMonthToGridCell(FSelectedYear,FSelectedMonth,LRow,LCol);
    RainfallGraphDialog.MonthlyGrid.CellRect(LCol,LRow);
    if (LRow > 0) and (LRow < RainfallGraphDialog.MonthlyGrid.RowCount - 6) and
         (LCol > 0) and (LCol < RainfallGraphDialog.MonthlyGrid.ColCount - 1) then
    begin
      FSystemFlag := True;
      RainfallGraphDialog.MonthlyGrid.Row := LRow;
      RainfallGraphDialog.MonthlyGrid.Col := LCol;
      FSystemFlag := False;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphValidator.GetPatchOriginalValue(AStation: IStationData; AHydroYearsIndex,AMonthIndex : Integer): double;
const OPNAME = 'TRainfallGraphValidator.GetPatchOriginalValue';
var
  lRainfallData     : IRainfallData;
  lYearlyData       : IYearlyData;
begin
  Result := -1;
  try
    lRainfallData := AStation.RainfallData;
    lYearlyData   := lRainfallData.GetHydroYearDataByIndex(AHydroYearsIndex);
    if  lYearlyData <> nil then
      Result := lYearlyData.MonthlyRainfall[AMonthIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoShowScaledDownValues(ASender: TObject);
const OPNAME = 'TRainfallGraphValidator.DoShowScaledDownValues';
var
  LRow,
  LCol,
  LCol1,
  LRow1                 : integer;
  LStation              : IStationData;
  LYearlyData           : IYearlyData;
  LRainfallData         : IRainfallData;
  LPatchParameterForm   : TPatchParameterDialog;
  LPatch                : IPatchData;
  LTempValue            : double;
  LTempSign             : WideString;
begin
   try
    LPatchParameterForm   := TPatchParameterDialog.CreateWithoutDFM(nil,FAppModules);
    try
      LCol1 := RainfallGraphDialog.MonthlyGrid.Col;
      LRow1 := RainfallGraphDialog.MonthlyGrid.Row;

      if RainfallGraphDialog.MonthlyGrid.Cells[LCol1,LRow1] = '*' then
      begin
        with RainfallGraphDialog do
        begin
          YearMonthToGridCell(FSelectedYear-1,FSelectedMonth,LRow,LCol);
          if (FSelectedMonth > 0 )and (FSelectedMonth <= 9) then
            LCol := FSelectedMonth + 3
          else
            LCol := FSelectedMonth - 9;

          LStation := (FAppModules.Model.ModelData as IRainfallModelData).
                        GetStationDataByID(FCurrentStationID);

          LStation.RainfallData.GetBaseDataForYearAndMonth(FSelectedYear,FSelectedMonth,LTempValue,LTempSign);
          if (LStation <> nil) then
          begin
            LPatch        := LStation.GetPatchWithID(FCurrentPatchID);
            LRainfallData := LPatch.RainfallData;
            LYearlyData   := LRainfallData.GetHydroYearDataByIndex(LRow);
            if (LTempValue <> NullFloat )then
            begin
              with  LPatchParameterForm  do
              begin
                lblStationNo.Caption := lStation.RainfallData.StationNumber ;
                edtSource.Text       := ' ' +FloatToStr(LTempValue);
                edtPatchValue.Text   := Format('%6.1f',[lYearlyData.MonthlyScaledDown[LCol]]);

                if (lPatch.Get_PatchScaledDownStatus(FCurrentStationID,FCurrentPatchID,FSelectedYear,FSelectedMonth))then
                  rgPatchSourceValues.ItemIndex := 0
                else
                  rgPatchSourceValues.ItemIndex := 1;
                LanguageHasChanged;
                ShowModal;
                if ModalResult = mrOK then
                begin
                  if  (rgPatchSourceValues.ItemIndex = 0) then
                  begin
                    if(not LPatch.Set_PatchScaledDownStatus(FCurrentStationID,FCurrentPatchID,FSelectedYear,FSelectedMonth,True)) then
                      ShowMessage(FAppModules.Language.GetString('Rainfall.CreatePatchChangelist'));

                    LYearlyData.MonthlyRainfall[LCol] := LYearlyData.MonthlyScaledDown[LCol];
                    PopulateGrid;
                  end
                  else if (rgPatchSourceValues.ItemIndex = 1) then
                  begin
                    if( not LPatch.Set_PatchScaledDownStatus(FCurrentStationID,FCurrentPatchID,FSelectedYear,FSelectedMonth,False)) then
                       ShowMessage(FAppModules.Language.GetString('Rainfall.CreatePatchChangelist'));

                    LYearlyData.MonthlyRainfall[LCol] := LTempValue;
                    PopulateGrid;
                  end
                  else
                  begin
                    ShowMessage(FAppModules.Language.GetString('Rainfall.ChooseValueToUse'));
                  end;
                end
                else
                  close;
              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LPatchParameterForm);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallGraphValidator.DoPatchChangeList(ASender: TObject);
const OPNAME = 'TRainfallGraphValidator.DoPatchChangeList';
var
  LStation              : IStationData;
  LPatch                : IPatchData;
  LYearlyData           : IYearlyData;
  LRainfallData         : IRainfallData;
  LChangeList           : IChangeList;
  LChangeGroup          : IChangeGroup;
  LParamField           : string;
  LKeyValues            : string;
  LIndex1,
  Lindex2,
  LColIndex             : Integer;
  LFieldIndex           : string;
  LChange               : double;
  lFieldProperty        : TAbstractFieldProperty;
  LYear                 : integer;
  LChangelistName       : string;
  LValue                : double;
  LSign                 : WideString;
begin
  LIndex1      := 0;
  try
    lStation := (FAppModules.Model.ModelData as IRainfallModelData).
                      GetStationDataByID(FCurrentStationID);

    if (lStation <> nil) then
    begin
      lPatch        := lStation.GetPatchWithID(FCurrentPatchID);
      if Assigned(LPatch) then
        lRainfallData := lPatch.RainfallData;

      LChangelistName := lStation.RainfallData.StationNumber + ' ('+ lPatch.PatchName +') Changelist';
      if (FAppModules.Changes.ChangeGroups.Count <> 0) then
      begin
        lChangeList := FAppModules.Changes.DoCreateNewChangeList;
        lChangeList.ChangeListName := LChangelistName;
      end
      else
      begin
        LChangeGroup               := FAppModules.Changes.DoCreateNewChangeGroup;
        LChangeGroup.GroupName     := FAppModules.Language.GetString('ChangeLists.NewPatchChangeGroup');;

        lChangeList                := FAppModules.Changes.DoCreateNewChangeList;
        lChangeList.ChangeListName := LChangelistName;
      end;

      if Assigned(lRainfallData) then
      begin
        while (LIndex1 < LRainfallData.HydroYearsCount - 1) do
        begin
          lYearlyData   := LRainfallData.GetHydroYearDataByIndex(lIndex1);
          for LIndex2 := 1 to 12 do
          begin
            if (LYearlyData.MonthlyScaledDown[LIndex2] <> NullFloat) then
            begin
              if (LIndex2 > 0 )and (LIndex2 <= 3) then
              begin
                LColIndex := LIndex2 + 9;
                LYear := LYearlyData.Year;
              end
              else
              begin
                LColIndex    := LIndex2 - 3;
                LYear := LYearlyData.Year +1;
              end;
              LStation.RainfallData.GetBaseDataForYearAndMonth(lYear,LColIndex,LValue,LSign);
              if (LValue <> NullFloat)then
              begin
                LFieldIndex    := IntToStr(LColIndex);
                LFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyPatchData');
                LChange        := lYearlyData.MonthlyRainfall[LIndex2];
                LKeyValues     := lPatch.GetKeyValues(lFieldProperty.FieldName, IntToStr(LYear ) + ',' + LFieldIndex);
                LParamField    := 'MonthlyPatchData';
                LChangeList.CreateNewParamChange(LParamField, LKeyValues,LFieldIndex,'Y',Format('%6.1f',[LChange]),'Scaled Down Values',False);
              end;
            end;
          end;
          LIndex1 := LIndex1 + 1;
        end;
      end;
    end;
    PopulateGrid;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallGraphValidator.isMonthlyScaledDownValuesTableEmpty:boolean;
const OPNAME = 'TRainfallGraphValidator.isMonthlyScaledDownValuesTableEmpty';
var
  LDataset : TAbstractModelDataset;
  LSQL     : string;
begin
  Result := False;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataset);
    try
      if Assigned(lDataset) then
      begin
        LSQL := 'Select PatchID,StationID  From RainfallScaledDownPatchValues' +
                ' Where '+
                ' PatchID   = ' + IntToStr(FCurrentPatchID)  +
                ' AND'+
                ' StationID = ' + IntToStr(FCurrentStationID);
        LDataset.SetSQL(lSQL);
        LDataset.DataSet.Open;
        if (LDataset.DataSet.Eof) then
          Result := TRUE;
        LDataset.DataSet.Close;
      end;
    finally
      FreeAndNil(lDataset);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

