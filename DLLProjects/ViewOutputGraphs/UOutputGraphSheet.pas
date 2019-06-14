//
//
//  UNIT      : Contains TOutputGraphSheet Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/01
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UOutputGraphSheet;

interface

uses
  VCLTee.Chart,
  VCL.ComCtrls,
  UDataSetType,
  UViewDataItem,
  UViewDataList,
  UHelpContexts,

  UAbstractObject,
  UAbstractComponent,
  UDataViewerSheet,
  VoaimsCom_TLB,
  UDataViewerMesgSheet;

type
  TGraphTreeItemDataObject = class(TAbstractAppObject)
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

  TOutputGraphSheet = class(TDataViewerMesgSheet)
  protected
    FToolBar: TAbstractToolBar;
    FChart: TChart;

    // Overriden from TAbstractTabSheet
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;

    function GetToolBar: TAbstractToolBar; override;

    // Overriden from TDataViewerSheet.
    procedure ClearDataViewer; override;
    procedure PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData); override;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); override;
  public
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure PopulateTreeView; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
  end;

implementation

uses
  System.UITypes,
  SysUtils,
  VCL.Controls,
  VCL.Graphics,
  Windows,
  VCL.Clipbrd,
  VCL.Printers,
  Contnrs,
  VCLTee.TeExport,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  UViewOutputChart,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TOutputGraphSheet.CreateMemberObjects;
const OPNAME = 'TOutputGraphSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'OutputGraph';
    FViewTypeConstant := 'OutputGraph';
    FChart := nil;
    FToolBar := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TOutputGraphSheet.AssignHelpContext;
const OPNAME = 'TOutputGraphSheet.AssignHelpContext';
begin
  try

    SetControlHelpContext(Self,HC_GraphView);
    SetControlHelpContext(FTreeView,HC_GraphViewTreeView);
    SetControlHelpContext(vcl.Controls.TControl(FChart),HC_GridViewGrid);
    SetControlHelpContext(FToolBar,HC_Toolbar);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputGraphSheet.DestroyMemberObjects;
const OPNAME = 'TOutputGraphSheet.DestroyMemberObjects';
begin
  try
    FreeAndNil(FChart);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TOutputGraphSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TOutputGraphSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := FToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphSheet.PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TOutputGraphSheet.PopulateTreeNodeDataObject';
var LGraphDatasetData: TGraphTreeItemDataObject;
begin
  try
    if Assigned(ADataObject) and
       Assigned(ADataObject.ViewDataNode) and
       (ADataObject.ViewDataNode.ViewDataSetCount > 0) then
    begin
      LGraphDatasetData := TGraphTreeItemDataObject.Create(FAppModules);
      LGraphDatasetData.PopulateTreeNodeDataObject(ADataObject.ViewDataNode.ViewID);
      ADataObject.Data  := LGraphDatasetData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphSheet.ClearDataViewer;
const OPNAME = 'TOutputGraphSheet.ClearDataViewer';
begin
  try
    if Assigned(FChart) then
    begin
      FChart.Parent := nil;
      FreeAndNil(FChart);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputGraphSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TOutputGraphSheet.PopulateDataViewer';
var
  LLanguageStr: string;
  LUnits: string;
  LCaption: string;
  LData: TGraphTreeItemDataObject;
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
                FChart := TViewOutputChart.Create(nil, FAppModules);
                FChart.Parent := self;
                FChart.Align := alClient;
                LGraphDisplayed := True;

                // Get the data object.
                LData := TGraphTreeItemDataObject(ADataObject.Data);

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
                TViewOutputChart(FChart).CreateSeries(
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

function TOutputGraphSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TOutputGraphSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphSheet.CanExport: boolean;
const OPNAME = 'TOutputGraphSheet.CanExport';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputGraphSheet.CanPrint: boolean;
const OPNAME = 'TOutputGraphSheet.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputGraphSheet.DoCopyToClipboard;
const OPNAME = 'TOutputGraphSheet.DoCopyToClipboard';
var
  LBitmap: vcl.Graphics.TBitmap;
  LBMPFormat: word;
  LData: THandle; //cardinal;
  LPalette: HPALETTE;
begin
  try

    // Create a bitmap for the clipboard output,
    // Generate the export format and store it on the clipboard
    if Assigned(FChart) then
    begin
      LBitmap := vcl.Graphics.TBitmap.Create;
      try
        TViewOutputChart(FChart).GenerateExportGraphOutput(LBitmap);
        LBitmap.SaveToClipboardFormat(LBMPFormat, LData, LPalette);
        ClipBoard.SetAsHandle(LBMPFormat, LData);
      finally
        FreeAndNil(LBitmap);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputGraphSheet.DoExport(AFileName: string = '');
const OPNAME = 'TOutputGraphSheet.DoExport';
//var
//  LBitmap: Graphics.TBitmap;
//  LExportFilename: string;
begin
  try

    // Create a bitmap for the clipboard output,
    // Generate the export format and save it to export file
    if Assigned(FChart) then
      TeeExport(Self, VCLTee.TeeProcs.TCustomTeePanel(FChart));
    {begin
      if FAppModules.GetExportFilename('.bmp',
        'BMP Files (*.bmp)|*.bmp|All Files (*.*)|*.*', LExportFilename) then
      begin
        LBitmap := Graphics.TBitmap.Create;
        try
          TViewOutputChart(FChart).GenerateExportGraphOutput(LBitmap);
          LBitmap.SaveToFile(LExportFilename);
        finally
          FreeAndNil(LBitmap);
        end;
      end;
    end;}

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputGraphSheet.DoPrint;
const OPNAME = 'TOutputGraphSheet.DoPrint';
begin
  try
    case Printer.Orientation of
      poPortrait  : FChart.PrintPortrait;
      poLandscape : FChart.PrintLandscape;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{ TGraphTreeItemDataObject }

constructor TGraphTreeItemDataObject.Create(AAppModules: TAppModules);
const OPNAME = 'TGraphTreeItemDataObject.Create';
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

procedure TGraphTreeItemDataObject.PopulateTreeNodeDataObject(AViewID: string);
const OPNAME = 'TGraphTreeItemDataObject.PopulateTreeNodeDataObject';
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

procedure TOutputGraphSheet.PopulateTreeView;
const OPNAME = 'TOutputGraphSheet.PopulateTreeView';
begin
  inherited PopulateTreeView;
  try
    DeleteEmptyParentNodes;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TOutputGraphSheet.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TOutputGraphSheet.StudyDataHasChanged';
var
  LIndex          : integer;
  LNode           : TTreeNode;
  LDataObject     : TViewDataTreeNodeData;
  LReservoir      : IReservoirData;
  LChannel        : IGeneralFlowChannel;
  LIrrigationArea : IIrrigationArea;
  LPowerPlant     : IPowerPlant;
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
            if(LNetworkItemName = 'RESERVOIR') and
             ((LDataObject.ViewDataNode.DataType = 'RESERVOIR') or
              (LDataObject.ViewDataNode.DataType = 'NODEWITHINFLOW') or
              (LDataObject.ViewDataNode.DataType = 'NODEWITHOUTINFLOW')) then
            begin
              LReservoir := (FAppModules.Model.ModelData as IYieldModelData).
                            NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[LIdentifier];
              if Assigned(LReservoir) then
              begin
                LDataObject.ViewDataNode.OverrideCaption := LReservoir.ReservoirConfigurationData.ReservoirName;
                LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
              end;
            end;
            if(LNetworkItemName = 'CHANNEL') and (LDataObject.ViewDataNode.DataType = 'CHANNEL') then
            begin
              try
                LChannel := (FAppModules.Model.ModelData as IYieldModelData).
                            NetworkElementData.ChannelList.ChannelByChannelNumber[LIdentifier];
                if Assigned(LChannel) then
                begin
                  LDataObject.ViewDataNode.OverrideCaption := LChannel.ChannelName;
                  LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
                end;
              finally
                LChannel := nil;
              end;
            end;
            if(LNetworkItemName = 'POWERPLANT') and (LDataObject.ViewDataNode.DataType = 'POWERPLANT') then
            begin
              try
                LPowerPlant := (FAppModules.Model.ModelData as IYieldModelData).
                               NetworkFeaturesData.PowerPlantList.PowerPlantByID[LIdentifier];
                if Assigned(LPowerPlant) then
                begin
                  LDataObject.ViewDataNode.OverrideCaption := LPowerPlant.FeatureName;
                  LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
                end;
              finally
                LPowerPlant := nil;
              end;
            end;
            if(LNetworkItemName = 'IRRIGATIONAREA') and (LDataObject.ViewDataNode.DataType = 'IRRIGATIONAREA') then
            begin
              try
                LIrrigationArea := (FAppModules.Model.ModelData as IYieldModelData).
                                   NetworkFeaturesData.IrrigationAreaList.IrrigationAreaByNodeNumber[LIdentifier];
                if Assigned(LIrrigationArea) then
                begin
                  LDataObject.ViewDataNode.OverrideCaption := LIrrigationArea.FeatureName;
                  LNode.Text := LDataObject.ViewDataNode.OverrideCaption;
                end;
              finally
                LIrrigationArea := nil;
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

