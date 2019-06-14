//
//
//  UNIT      : Contains TGridOutputEditorSheet Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/02
//  COPYRIGHT : Copyright © 2002 DWAF
//
//                                                 
unit UGridOutputEditorSheet;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,

  UAbstractObject,
  UViewDataItem,
  UGridOutputEditorGrid,
  UDataViewerSheet,
  UDataViewerMesgSheet,
  UAbstractGridData,
  UGridActionObject,
  UAbstractComponent,
  UHelpContexts,
  UGridOutputEditorMenuItemManager;

type
  TDataViewerMode = (dvmNone, dvmGrid, dvmDialog);
  TGridOutputEditorSheet = class(TDataViewerMesgSheet)
  protected
    FGrid: TGridOutputEditor;
    procedure CreateMemberObjects; override;
    procedure SetCurrentDataSet(ACurrentDataset: TAbstractModelDataset); override;
    function GetToolBar: TAbstractToolBar; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); override;
    procedure CreateGridData(var ADataViewerMode:TDataViewerMode;ADataObject: TViewDataTreeNodeData);
    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean); override;
    procedure DoRotateView(ASender: TObject);

    function GetMenuItemManager: TGridOutputEditorMenuItemManager;
    function FindDatasetRecord(AKeyProperties: TStrings): Boolean;
    procedure PopulateGridMonthsNames;
    procedure AssignHelpContext; override;
  public
    procedure DoOnHint; override;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function Initialise: boolean; override;
    function DoGridAction(AGridActionObject: TGridActionObject): boolean; override;
    procedure ProcessCustomEvent(AData: TObject); override;
    property MenuItemManager: TGridOutputEditorMenuItemManager read GetMenuItemManager;
  end;

implementation

uses
  SysUtils,
  Variants,

  VoaimsCom_TLB,
  UGridOutputDataDataSets,
  UGridEditorStringGrid,
  UErrorHandlingOperations,
  UTreeViewTabSheet;

procedure TGridOutputEditorSheet.CreateMemberObjects;
const OPNAME = 'TGridOutputEditorSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;

    ShowHint := False;
    ParentShowHint := False;

    FTabCaptionKey    := 'GridEditor';
    FViewTypeConstant := FAppModules.Language.GetString('ViewType.OutputGrid');

    FGrid := TGridOutputEditor.Create(self, FAppModules);
    FGrid.Parent := self;
    FGrid.Align  := alClient;

    FMenuItemManager := TGridOutputEditorMenuItemManager.Create(FAppModules);

    MenuItemManager.ToolBar.PivotBtn.OnClick := DoRotateView;
    MenuItemManager.SetPivotState(False);

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TGridOutputEditorSheet.AssignHelpContext;
const OPNAME = 'TGridOutputEditorSheet.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,HC_GridView);
    SetControlHelpContext(FTreeView,HC_GridViewTreeView);
    SetControlHelpContext(FGrid,HC_GridViewGrid);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridOutputEditorSheet.Initialise: boolean;
const OPNAME = 'TGridOutputEditorSheet.Initialise';
begin
  Result := inherited Initialise;
  try
    FGrid.Initialise;
    FMenuItemManager.Initialise;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridOutputEditorSheet.SetCurrentDataSet(ACurrentDataset: TAbstractModelDataset);
const OPNAME = 'TGridOutputEditorSheet.SetCurrentDataSet';
begin
  try
    inherited SetCurrentDataSet(ACurrentDataset);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorSheet.ProcessCustomEvent(AData: TObject);
const OPNAME = 'TGridOutputEditorSheet.ProcessCustomEvent';
begin
  try
    case TGridOutputEditorMenuData(AData).Action of
      geRotateView : DoRotateView(nil);
    else
      raise Exception.CreateFmt(FAppModules.Language.GetString('TGridEditorSheet.UnknownGridEditorEventType')
                                , [integer(TGridOutputEditorMenuData(AData).Action)]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorSheet.DoRotateView(ASender: TObject);
const OPNAME = 'TGridOutputEditorSheet.DoRotateView';
begin
  try
    if Assigned(FGrid.Grid) and FGrid.Grid.IsVisible then
    begin
      FGrid.DoRotateView(nil);
      FGrid.PopulateAverageRow;
      PopulateGridMonthsNames;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorSheet.GetToolBar: TAbstractToolBar;
const OPNAME = 'TGridOutputEditorSheet.GetToolBar';
begin
  Result := nil;
  try
    Result := MenuItemManager.ToolBar;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorSheet.LanguageHasChanged: boolean;
const OPNAME = 'TGridOutputEditorSheet.LanguageHasChanged';
begin
  Result := False;
  try
    inherited LanguageHasChanged;
    FGrid.LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorSheet.ClearDataViewer;
const OPNAME = 'TGridOutputEditorSheet.ClearDataViewer';
begin
  try
    inherited ClearDataViewer;
    if Assigned(FGrid) then
      FGrid.ClearDataViewer;
 //     FGrid.Grid.ClearDataViewer;
  //    FreeAndNil(FGrid.Grid);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridOutputEditorSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TGridOutputEditorSheet.PopulateDataViewer';
var
  LDataViewerMode:TDataViewerMode;
begin
  try
    FModelTabSheetName :=  mtsnNone;
    inherited PopulateDataViewer(ADataObject);
    if Assigned(ADataObject) and Assigned(ADataObject.ViewDataNode) then
    begin
      CreateGridData(LDataViewerMode,ADataObject);
      if Assigned(FGrid.Grid) then
      begin
        FGrid.Grid.PopulateDataViewer;
        FGrid.Visible := FGrid.Grid.IsVisible;
        FGrid.PopulateAverageRow;
        PopulateGridMonthsNames;
        FGrid.Grid.UpdateHintText;
      end;
      case LDataViewerMode of
        dvmNone   :
          begin
            MenuItemManager.SetPivotState(False);
            ShowNoDataMessage;
          end;
        dvmGrid   : MenuItemManager.SetPivotState(Assigned(FGrid.Grid) and FGrid.Visible);
        dvmDialog :
        begin
          FModelTabSheetName :=  mtsnInput;
          MenuItemManager.SetPivotState(False);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorSheet.CreateGridData(var ADataViewerMode:TDataViewerMode;ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TGridOutputEditorSheet.CreateGridData';
var
  LIndex : integer;
  LClassMatchIndex : integer;
  LClassMatch : boolean;
begin
  ADataViewerMode := dvmNone;
  try

    // Make sure that the old object has been destroyed.
    FGrid.GridData := nil;

    // Cleanup olde visible elements external to self
    if not Assigned(FGrid.Grid) then
    begin
      for LIndex := 0 to Self.ControlCount - 1 do
      begin
        LClassMatch := false;
        for LClassMatchIndex := 0 to Self.ComponentCount - 1 do
          if Self.Components[LClassMatchIndex].ClassName = Self.Controls[LIndex].ClassName then
            LClassMatch := True;

        // Ask if the control parent is component matched to self
        if not LClassMatch then
          // cleanup last visible control(s) so new active control(s) is not compromised by over/inder lap
          Self.Controls[LIndex].Visible := false;
      end;
    end;

    // Process as normal data set grid data.
    if (ADataObject.ViewDataNode.ShowSQL) and
       (ADataObject.ViewDataNode.ViewDataSetCount > 0) then
    begin
      FGrid.GridData := TGridOutputDataSets.Create(FAppModules);
      TGridOutputDataSets(FGrid.GridData).ConstructDataSets(ADataObject.ViewDataNode);
      FGrid.ViewMode := FGrid.LastNormalViewMode;
      ADataViewerMode := dvmGrid;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorSheet.GetMenuItemManager: TGridOutputEditorMenuItemManager;
const OPNAME = 'TGridOutputEditorSheet.GetMenuItemManager';
begin
  Result := nil;
  try
    Result := TGridOutputEditorMenuItemManager(FMenuItemManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorSheet.DoGridAction(AGridActionObject: TGridActionObject): boolean;
const OPNAME = 'TGridOutputEditorSheet.DoGridAction';
begin
  Result := False;
  try
    if inherited DoGridAction(AGridActionObject) then
    begin
       case AGridActionObject.Action of
         gaEdit, gaView :
           Result := FindDatasetRecord(AGridActionObject.DatasetProperties);
       end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorSheet.FindDatasetRecord(AKeyProperties: TStrings): Boolean;
const OPNAME = 'TGridOutputEditorSheet.FindDatasetRecord';
var
  LKeyValues: Variant;
  LKeyFields: string;
  LCount: integer;
begin
  Result := False;
  try
    if (AKeyProperties.Count = 0) then
      Exit;
    if Assigned(FCurrentDataset) and
       Assigned(FCurrentDataset.DataSet()) and
       FCurrentDataset.DataSet.Active then
    begin
      LKeyValues := VarArrayCreate([0, AKeyProperties.Count-1], varOleStr);
      LKeyFields := '';
      for LCount := 0 to AKeyProperties.Count - 1 do
      begin
        LKeyFields := LKeyFields + AKeyProperties.Names[LCount] + ';';
        LKeyValues[LCount] := AKeyProperties.Values[AKeyProperties.Names[LCount]];
      end;
      Delete(LKeyFields, Length(LKeyFields), 1);
      Result := FCurrentDataset.DataSet.Locate(LKeyFields, LKeyValues, []);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorSheet.DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TGridOutputEditorSheet.DoTreeNodeAboutToChange';
begin
  try
    MenuItemManager.SetPivotState(False);
    if Assigned(FGrid.Grid) then
      FGrid.SaveState;
    if (FGrid.EditState <> esBrowse) then
      AAllowChange := False;
    inherited DoTreeNodeAboutToChange(ASender, ANode, AAllowChange);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorSheet.DoOnHint;
const OPNAME = 'TGridOutputEditorSheet.DoOnHint';
begin
  try
    if Assigned(FGrid.Grid) then
      FGrid.UpdateHintText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorSheet.StudyHasChanged: boolean;
const OPNAME = 'TGridOutputEditorSheet.StudyHasChanged';
begin
  Result := False;
  try
    Result := inherited StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TGridOutputEditorSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    if Assigned(FGrid.Grid) then
      Result := FGrid.Grid.IsVisible
    else
      Result := FAppModules.Model.CanCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorSheet.CanExport: boolean;
const OPNAME = 'TGridOutputEditorSheet.CanExport';
begin
  Result := False;
  try
    if Assigned(FGrid.Grid) then
      Result := FGrid.Grid.IsVisible
    else
      Result := FAppModules.Model.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TGridOutputEditorSheet.CanPrint: boolean;
const OPNAME = 'TGridOutputEditorSheet.CanPrint';
begin
  Result := False;
  try
    if Assigned(FGrid.Grid) then
      Result := FGrid.Grid.IsVisible
    else
      Result := FAppModules.Model.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorSheet.DoCopyToClipboard;
const OPNAME = 'TGridOutputEditorSheet.DoCopyToClipboard';
begin
  try
    if Assigned(FGrid.Grid) then
      FGrid.DoCopyToClipboard
    else
      FAppModules.Model.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TGridOutputEditorSheet.DoExport(AFileName: string = '');
const OPNAME = 'TGridOutputEditorSheet.DoExport';
begin
  try
    if Assigned(FGrid.Grid) then
      FGrid.DoExport(AFileName)
    else
      FAppModules.Model.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridOutputEditorSheet.DoPrint;
const OPNAME = 'TGridOutputEditorSheet.DoPrint';
begin
  try

    if Assigned(FGrid.Grid) then
      FGrid.DoPrint(FTreeView.Selected.Text)
    else
      FAppModules.Model.DoPrint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TGridOutputEditorSheet.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TGridOutputEditorSheet.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if(UpperCase(AFieldName) = 'MONTH') then
      PopulateGridMonthsNames;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TGridOutputEditorSheet.PopulateGridMonthsNames;
const OPNAME = 'TGridOutputEditorSheet.PopulateGridMonthsNames';
var
  LMonthNamesList : TStringList;
  LIndex: integer;
begin
  try
    LMonthNamesList := TStringList.Create;
    try
      if (FGrid <> nil) and (FAppModules.Model <> nil) and (FAppModules.Model.ModelName = CYield) then
      begin
        for LIndex := 1 to 12 do
        begin
          LMonthNamesList.Add((FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData.MonthNameByIndex[LIndex]);
        end;
        FGrid.PopulateGridMonthsNames(LMonthNamesList);
      end;
    finally
      LMonthNamesList.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
