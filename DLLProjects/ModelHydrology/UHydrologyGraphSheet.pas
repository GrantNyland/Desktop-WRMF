//
//
//  UNIT      : Contains THydrologyGraphSheet Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/01
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UHydrologyGraphSheet;

interface

uses
  Chart,
  ComCtrls,
  UDataSetType,
  UViewDataItem,
  UViewDataList,
  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UDataViewerSheet,
  UDataViewerMesgSheet;

type
  THydrologyGraphTreeItemDataObject = class(TAbstractAppObject)
  protected
    FLeftAxisCaption: string;
    FLeftAxisFormat: string;
    FLeftAxisUnits: string;
    FBottomAxisCaption: string;
    FBottomAxisFormat: string;
    FBottomAxisUnits: string;
    FGraphHint: string;
  public
    constructor Create(AAppModules: TAppModules);
    procedure PopulateTreeNodeDataObject(AViewID: string);
    property LeftAxisCaption: string read FLeftAxisCaption write FLeftAxisCaption;
    property LeftAxisFormat: string read FLeftAxisFormat write FLeftAxisFormat;
    property LeftAxisUnits: string read FLeftAxisUnits write FLeftAxisUnits;
    property BottomAxisCaption: string read FBottomAxisCaption write FBottomAxisCaption;
    property BottomAxisFormat: string read FBottomAxisFormat write FBottomAxisFormat;
    property BottomAxisUnits: string read FBottomAxisUnits write FBottomAxisUnits;
    property GraphHint: string read FGraphHint write FGraphHint;
  end;

  THydrologyGraphSheet = class(TDataViewerMesgSheet)
  protected
    FToolBar: TAbstractToolBar;
    FChart: TChart;

    // Overriden from TAbstractTabSheet
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure DeleteEmptyParentNodes;  override;

    function GetToolBar: TAbstractToolBar; override;

    // Overriden from TDataViewerSheet.
    procedure ClearDataViewer; override;
    procedure PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData); override;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); override;
  public
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure PopulateTreeView; override;
    function CanCopyToClipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToClipboard; override;
    procedure DoExport; override;
    procedure DoPrint; override;
  end;

implementation

uses
  SysUtils,
  Controls,
  Graphics,
  Windows,
  Clipbrd,
  Printers,
  Contnrs,
  TeExport,
  UAbstractReservoirData,
  UAbstractYieldModelDataObject,
  UHydrologyViewDataChart,
  UVaalDBMSMenuEventType,
  UErrorHandlingOperations;

procedure THydrologyGraphSheet.CreateMemberObjects;
const OPNAME = 'THydrologyGraphSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'Graph';
    FViewTypeConstant := FAppModules.Language.GetString('TabCaption.Graph');
    FChart := nil;
    FToolBar := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure THydrologyGraphSheet.AssignHelpContext;
const OPNAME = 'THydrologyGraphSheet.AssignHelpContext';
begin
  try

    SetControlHelpContext(Self,HC_GraphView);
    SetControlHelpContext(FTreeView,HC_GraphViewTreeView);
    SetControlHelpContext(FChart,HC_GridViewGrid);
    SetControlHelpContext(FToolBar,HC_Toolbar);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyGraphSheet.DestroyMemberObjects;
const OPNAME = 'THydrologyGraphSheet.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChart);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function THydrologyGraphSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'THydrologyGraphSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := FToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyGraphSheet.PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'THydrologyGraphSheet.PopulateTreeNodeDataObject';
var LGraphDatasetData: THydrologyGraphTreeItemDataObject;
begin
  try
    if Assigned(ADataObject) and
       Assigned(ADataObject.ViewDataNode) and
       (ADataObject.ViewDataNode.ViewDataSetCount > 0) then
    begin
      LGraphDatasetData := THydrologyGraphTreeItemDataObject.Create(FAppModules);
      LGraphDatasetData.PopulateTreeNodeDataObject(ADataObject.ViewDataNode.ViewID);
      ADataObject.Data  := LGraphDatasetData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyGraphSheet.ClearDataViewer;
const OPNAME = 'THydrologyGraphSheet.ClearDataViewer';
begin
  try
    if Assigned(FChart) then
    begin
      FChart.Parent := nil;
      FreeAndNil(FChart);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyGraphSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'THydrologyGraphSheet.PopulateDataViewer';
var
  LLanguageStr: string;
  LUnits: string;
  LCaption: string;
  LData: THydrologyGraphTreeItemDataObject;
  LDataset      : TAbstractModelDataset;
  LDataSetIndex : integer;
  LGraphDisplayed: boolean;
begin
  inherited PopulateDataViewer(ADataObject);
  try
    LGraphDisplayed := False;
    if Assigned(ADataObject) and
       Assigned(ADataObject.ViewDataNode) and
       (ADataObject.ViewDataNode.ViewDataSetCount > 0) and
       (ADataObject.ViewDataNode.ShowSQL) then
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
      if Assigned(LDataSet) then
      begin
        for LDataSetIndex := 0 to ADataObject.ViewDataNode.ViewDataSetCount - 1 do
        begin
          LDataSet.SetSQL(ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].ViewSQL);
          LDataSet.DataSet.Close;
          FAppModules.StudyArea.SetDefaultParams(LDataSet);
          if (ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].ParamCount > 0) then
            LDataSet.SetParams(
              ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].ParamNames,
              ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].ParamValues);
          if LDataSet.AreAllParamsBound then
          begin
            LDataSet.DataSet.Open;
            try
              if (LDataSet.DataSet.RecordCount > 0) then
              begin
                if Assigned(FChart) then
                begin
                  FChart.Parent := nil;
                  FreeAndNil(FChart);
                end;
                FChart := TViewHydrologyDataChart.Create(nil, FAppModules);
                FChart.Parent := self;
                FChart.Color := clWhite;
                FChart.Align := alClient;
                LGraphDisplayed := True;

                // Get the data object.
                LData := THydrologyGraphTreeItemDataObject(ADataObject.Data);

                // Set chart text and Construct units string.
                FChart.Title.Text.Text := FTreeView.Selected.Text;

                LCaption     := '';
                LLanguageStr := LData.FLeftAxisCaption;
                LUnits       := LData.FLeftAxisUnits;
                if(Trim(LLanguageStr) <> '') then
                  LCaption     := FAppModules.Language.GetString(LLanguageStr);
                if(Trim(LUnits) <> '') then
                   LCaption := LCaption + ' (' + LUnits +')';
                FChart.LeftAxis.Title.Caption := LCaption;

                LCaption     := '';
                LLanguageStr := LData.FBottomAxisCaption;
                LUnits       := LData.FBottomAxisUnits;
                if(Trim(LLanguageStr) <> '') then
                  LCaption     := FAppModules.Language.GetString(LLanguageStr);
                if(Trim(LUnits) <> '') then
                   LCaption := LCaption + ' (' + LUnits +')';
                FChart.BottomAxis.Title.Caption := LCaption;
                FChart.LeftAxis.AxisValuesFormat   := LData.FLeftAxisFormat;
                FChart.BottomAxis.AxisValuesFormat := LData.FBottomAxisFormat;

                FChart.ShowHint := False;
                LLanguageStr := LData.GraphHint;
                if (LLanguageStr <> '') then
                begin
                  FChart.ShowHint := True;
                  FChart.Hint := FAppModules.Language.GetString(LLanguageStr);
                end;

                // Create the series.
                TViewHydrologyDataChart(FChart).CreateSeries(
                  TChartSeriesType(ADataObject.ViewDataNode.ViewDataSet[LDataSetIndex].SQLType), LDataset.DataSet);
              end;
            finally
              LDataSet.DataSet.Close;
            end;
          end;
        end;
      end;
    end;
    if not LGraphDisplayed then
       ShowNoDataMessage;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydrologyGraphSheet.CanCopyToClipboard: boolean;
const OPNAME = 'THydrologyGraphSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyGraphSheet.CanExport: boolean;
const OPNAME = 'THydrologyGraphSheet.CanExport';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyGraphSheet.CanPrint: boolean;
const OPNAME = 'THydrologyGraphSheet.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyGraphSheet.DoCopyToClipboard;
const OPNAME = 'THydrologyGraphSheet.DoCopyToClipboard';
var
  LBitmap: Graphics.TBitmap;
  LBMPFormat: word;
  LData: cardinal;
  LPalette: HPALETTE;
begin
  try

    // Create a bitmap for the clipboard output,
    // Generate the export format and store it on the clipboard
    if Assigned(FChart) then
    begin
      LBitmap := Graphics.TBitmap.Create;
      try
        TViewHydrologyDataChart(FChart).GenerateExportGraphOutput(LBitmap);
        LBitmap.SaveToClipboardFormat(LBMPFormat, LData, LPalette);
        ClipBoard.SetAsHandle(LBMPFormat, LData);
      finally
        FreeAndNil(LBitmap);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyGraphSheet.DoExport;
const OPNAME = 'THydrologyGraphSheet.DoExport';
//var
//  LBitmap: Graphics.TBitmap;
//  LExportFilename: string;
begin
  try

    // Create a bitmap for the clipboard output,
    // Generate the export format and save it to export file
    if Assigned(FChart) then
      TeeExport(Self, FChart);

    {begin
      if FAppModules.GetExportFilename('.bmp',
        'BMP Files (*.bmp)|*.bmp|All Files (*.*)|*.*', LExportFilename) then
      begin
        LBitmap := Graphics.TBitmap.Create;
        try
          TViewHydrologyDataChart(FChart).GenerateExportGraphOutput(LBitmap);
          LBitmap.SaveToFile(LExportFilename);
        finally
          FreeAndNil(LBitmap);
        end;
      end;
    end;}

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydrologyGraphSheet.DoPrint;
const OPNAME = 'THydrologyGraphSheet.DoPrint';
begin
  try
    case Printer.Orientation of
      poPortrait  : FChart.PrintPortrait;
      poLandscape : FChart.PrintLandscape;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ THydrologyGraphTreeItemDataObject }

constructor THydrologyGraphTreeItemDataObject.Create(AAppModules: TAppModules);
const OPNAME = 'THydrologyGraphTreeItemDataObject.Create';
begin
  try
    inherited Create(AAppModules);
    FLeftAxisCaption      := '';
    FLeftAxisFormat       := '';
    FLeftAxisUnits        := '';
    FBottomAxisCaption    := '';
    FBottomAxisFormat     := '';
    FBottomAxisUnits      := '';
    FGraphHint            := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyGraphTreeItemDataObject.PopulateTreeNodeDataObject(AViewID: string);
const OPNAME = 'THydrologyGraphTreeItemDataObject.PopulateTreeNodeDataObject';
var LFieldPropertyX, LFieldPropertyY, LFieldPropertyGraph: TAbstractFieldProperty;
begin
  try
    if Assigned(FAppModules.FieldProperties()) then
    begin
      LFieldPropertyX     := FAppModules.FieldProperties.FieldProperty(AViewID + 'X');
      LFieldPropertyY     := FAppModules.FieldProperties.FieldProperty(AViewID + 'Y');
      LFieldPropertyGraph := FAppModules.FieldProperties.FieldProperty(AViewID + 'G');
      if Assigned(LFieldPropertyGraph) then
      begin
        GraphHint := LFieldPropertyGraph.FieldDescription;
      end;
      if Assigned(LFieldPropertyX) then
      begin
        BottomAxisCaption := LFieldPropertyX.FieldDescription;
        BottomAxisUnits   := LFieldPropertyX.FieldUnits;
        BottomAxisFormat  := LFieldPropertyX.FormatStringGraph;
      end;
      if Assigned(LFieldPropertyY) then
      begin
        LeftAxisCaption := LFieldPropertyY.FieldDescription;
        LeftAxisUnits   := LFieldPropertyY.FieldUnits;
        LeftAxisFormat  := LFieldPropertyY.FormatStringGraph;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyGraphSheet.PopulateTreeView;
const OPNAME = 'THydrologyGraphSheet.PopulateTreeView';
begin
  inherited PopulateTreeView;
  try
    DeleteEmptyParentNodes;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure THydrologyGraphSheet.DeleteEmptyParentNodes;
const OPNAME = 'THydrologyGraphSheet.DeleteEmptyParentNodes';
var
  LCountainer: TObjectList;
  LCount: integer;
  LNode: TTreeNode;
begin
  try
    LCountainer := TObjectList.Create(False);
    try
      LCountainer.Clear;
      for LCount := 0 to  FTreeView.Items.Count -1 do
      begin
        if (FTreeView.Items.Item[LCount].Level = 1) and (FTreeView.Items.Item[LCount].Count = 0) then
           LCountainer.Add(FTreeView.Items.Item[LCount]);
      end;
      for LCount := 0 to  LCountainer.Count -1 do
      begin
        LNode := TTreeNode(LCountainer.Items[LCount]);
        FTreeView.Items.Delete(LNode);
      end;
    finally
      LCountainer.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function THydrologyGraphSheet.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'THydrologyGraphSheet.StudyDataHasChanged';
var
  LIndex          : integer;
  LNode           : TTreeNode;
  LDataObject     : TViewDataTreeNodeData;
  LReservoir      : TAbstractReservoirData;
  LChannel        : TAbstractGeneralFlowChannel;
  LIrrigationArea : TAbstractIrrigationArea;
  LPowerPlant     : TAbstractNetworkFeature;
  LNetworkItemName: string;
  LIdentifier     : integer;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    LNetworkItemName := '';
    if(AFieldName = 'ReservoirName') then LNetworkItemName := 'RESERVOIR';
    if(AFieldName = 'ChannelName') then LNetworkItemName := 'CHANNEL';
    if(AFieldName = 'PowerPlantName') then LNetworkItemName := 'POWERPLANT';
    if(AFieldName = 'AreaName') then LNetworkItemName := 'IRRIGATIONAREA';

    if(LNetworkItemName <> '') then
    begin
      for LIndex := 0 to FTreeViewNodes.Count -1 do
      begin
        LNode := TTreeNode(FTreeViewNodes.Objects[LIndex]);
        if Assigned(LNode) then
        begin
          if(LNode.Level = 0) then Continue;

          LDataObject := TViewDataTreeNodeData(LNode.Data);
          if Assigned(LDataObject) and Assigned(LDataObject.ViewDataNode) then
          begin
            LIdentifier := LDataObject.ViewDataNode.Weighting;
            if(LNetworkItemName = 'RESERVOIR') and (LDataObject.ViewDataNode.DataType = 'RESERVOIR') then
            begin
              LReservoir := TAbstractYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LIdentifier];
              if Assigned(LReservoir) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := LReservoir.ReservoirConfigurationData.ReservoirName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'CHANNEL') and (LDataObject.ViewDataNode.DataType = 'CHANNEL') then
            begin
              LChannel := TAbstractYieldModelDataObject(FAppModules.Model.ModelData).
                          NetworkElementData.ChannelList.ChannelByChannelNumber[LIdentifier];
              if Assigned(LChannel) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := LChannel.ChannelName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'POWERPLANT') and (LDataObject.ViewDataNode.DataType = 'POWERPLANT') then
            begin
              LPowerPlant := TAbstractYieldModelDataObject(FAppModules.Model.ModelData).
                             NetworkFeaturesData.PowerPlantList.PowerPlantByID[LIdentifier];
              if Assigned(LPowerPlant) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := LPowerPlant.FeatureName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'IRRIGATIONAREA') and (LDataObject.ViewDataNode.DataType = 'IRRIGATIONAREA') then
            begin
              LIrrigationArea := TAbstractYieldModelDataObject(FAppModules.Model.ModelData).
                                 NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByID[LIdentifier];
              if Assigned(LIrrigationArea) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := LIrrigationArea.FeatureName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;

          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

