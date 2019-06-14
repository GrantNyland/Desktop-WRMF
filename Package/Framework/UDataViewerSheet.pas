//
//
//  UNIT      : Contains TDataViewerSheet Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/04/12
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDataViewerSheet;

interface

uses
  vcl.ComCtrls,
  UViewDataItem,
  UAbstractObject,
  UGridActionObject,
  UTreeViewTabSheet;

type
  TDataViewerSheet = class(TTreeViewTabSheet)
  protected
    FStudyIsChanging: boolean;
    FViewTypeConstant: string;
    FCurrentDataset: TAbstractModelDataset;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData); virtual;
    procedure DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean); virtual;
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode); virtual;
    procedure ClearDataViewer; virtual;
    procedure PopulateDataViewer(ADataObject: TViewDataTreeNodeData); virtual;
    procedure SetCurrentDataSet(ACurrentDataset: TAbstractModelDataset); virtual;
  public
    procedure PopulateTreeView; override;
    function DoJumpRequest(AViewDataNode: TViewDataNode): boolean; override;
    function DoGridAction(AGridActionObject: TGridActionObject): boolean; override;
    function CanTabChange: boolean; override;
    procedure TabHasChanged; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    property ViewTypeConstant: string read FViewTypeConstant write FViewTypeConstant;
    property CurrentDataset: TAbstractModelDataset read FCurrentDataset write SetCurrentDataSet;
    property StudyIsChanging: boolean read FStudyIsChanging write FStudyIsChanging;
  end;

implementation

uses
  SysUtils,
  Classes,
  db,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TDataViewerSheet }

procedure TDataViewerSheet.CreateMemberObjects;
const OPNAME = 'TDataViewerSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FStudyIsChanging     := False;
    FViewTypeConstant    := '';
    FCurrentDataset      := nil;
    FTreeView.ReadOnly   := True;
    FTreeView.OnChanging := DoTreeNodeAboutToChange;
    FTreeView.OnChange   := DoTreeNodeHasChanged;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDataViewerSheet.DestroyMemberObjects;
const OPNAME = 'TDataViewerSheet.DestroyMemberObjects';
begin
  try
    if Assigned(FCurrentDataset) and
       Assigned(FCurrentDataset.DataSet()) then
      if (FCurrentDataset.DataSet.State = dsEdit) or
         (FCurrentDataset.DataSet.State = dsInsert) then
        FCurrentDataset.DataSet.Cancel;
    FreeAndNil(FCurrentDataset);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDataViewerSheet.SetCurrentDataSet(ACurrentDataset: TAbstractModelDataset);
const OPNAME = 'TDataViewerSheet.SetCurrentDataSet';
begin
  try
    FCurrentDataset := ACurrentDataset;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataViewerSheet.PopulateTreeView;
const OPNAME = 'TDataViewerSheet.PopulateTreeView';
var
  LIndex: integer;
  LNode:TTreeNode;
begin
  inherited PopulateTreeView;
  try
    FTreeView.Items.BeginUpdate;
    try
      FAppModules.Model.ViewData.PopulateTreeNodes(FTreeView.Items, FViewTypeConstant);
      for LIndex := 0 to FTreeViewNodes.Count - 1 do
      begin
        LNode := NodeByIndex[LIndex];
        if Assigned(LNode) and Assigned(LNode.Data) then
          PopulateTreeNodeDataObject(TViewDataTreeNodeData(LNode.Data));
      end;
    finally
      FTreeView.Items.EndUpdate;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataViewerSheet.PopulateTreeNodeDataObject(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TDataViewerSheet.PopulateTreeNodeDataObject';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataViewerSheet.CanTabChange: boolean;
const OPNAME = 'TDataViewerSheet.CanTabChange';
begin
  Result := True;
  try
    if Assigned(FAppModules.Model()) then
      FAppModules.Model.ViewData.CurrentTreeNodeData := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDataViewerSheet.TabHasChanged;
const OPNAME = 'TDataViewerSheet.TabHasChanged';
begin
  try
    if Assigned(FTreeView.Selected) then
      if Assigned(FTreeView.Selected.Data) then
         if Assigned(FAppModules.Model()) then
            FAppModules.Model.ViewData.CurrentTreeNodeData := FTreeView.Selected.Data;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataViewerSheet.LanguageHasChanged: boolean;
const OPNAME = 'TDataViewerSheet.LanguageHasChanged';
var
  LCount: integer;
  LDataObject: TViewDataTreeNodeData;
  LCurrentNode: TTreeNode;
  LangString: string;
begin
  Result := False;
  try
    inherited LanguageHasChanged;
    for LCount := 0 to FTreeViewNodes.Count - 1 do
    begin
      LCurrentNode := TTreeNode(FTreeViewNodes.Objects[LCount]);
      LDataObject := TViewDataTreeNodeData(LCurrentNode.Data);
      if LDataObject.ViewDataNode.IsLanguageSwitchable then
      begin
        LangString := 'ViewData.' + LDataObject.ViewDataNode.ViewID;
        LCurrentNode.Text := FAppModules.Language.GetString(LangString);
      end else begin
        LCurrentNode.Text := LDataObject.ViewDataNode.OverrideCaption;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataViewerSheet.StudyHasChanged: boolean;
const OPNAME = 'TDataViewerSheet.StudyHasChanged';
var LCurrentNodeText: string;
begin
  Result := False;
  try
    FStudyIsChanging := True;
    try
      if Assigned(FTreeView.Selected) then
        LCurrentNodeText := FTreeView.Selected.Text;
      inherited StudyHasChanged;
      ClearDataViewer;
      PopulateTreeView;
      LanguageHasChanged;
      SelectNodeByText(LCurrentNodeText);
      Result := True;
    finally
      FStudyIsChanging := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataViewerSheet.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDataViewerSheet.StudyDataHasChanged';
//var
//  LCanChange: boolean;
begin
  Result := False;
  try
    {if Assigned(FAppModules.FieldProperties.LastFieldToChange) and
       Assigned(FAppModules.FieldProperties.LastFieldToChange.FieldProperty) then
    begin
      if (FAppModules.FieldProperties.LastFieldToChange.FieldProperty.FieldName = 'ReservoirName')  or
         (FAppModules.FieldProperties.LastFieldToChange.FieldProperty.FieldName = 'ChannelName') then
//      begin
        StudyHasChanged;
//      end else begin
//        if (FAppModules.FieldProperties.LastFieldToChange.ChangedBy <> FViewTypeConstant) then
//        begin
 //         LCanChange := True;
 //         DoTreeNodeAboutToChange(nil, nil, LCanChange);
//          if LCanChange then
//            DoTreeNodeHasChanged(nil, nil);
//        end;
//      end;
    end;}
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataViewerSheet.DoJumpRequest(AViewDataNode: TViewDataNode): boolean;
const OPNAME = 'TDataViewerSheet.DoJumpRequest';
var
  LCount: integer;
  LDataObject: TViewDataTreeNodeData;
  LCurrentNode, LSelectedParentNode, LSelectedNode: TTreeNode;
begin
  Result := False;
  try
    if Assigned(AViewDataNode) and (AViewDataNode.ClassName = 'TViewDataNode') then
    begin

      // Try and find the required parent node.
      LSelectedParentNode := nil;
      LSelectedNode := nil;
      for LCount := 0 to FTreeViewNodes.Count - 1 do
      begin
        LCurrentNode := TTreeNode(FTreeViewNodes.Objects[LCount]);
        LDataObject := TViewDataTreeNodeData(LCurrentNode.Data);
        if Assigned(LDataObject) and
           Assigned(LDataObject.ViewDataNode) then
        begin
          if (LDataObject.ViewDataNode.ViewID = AViewDataNode.ViewID) then
          begin
            LSelectedParentNode := LCurrentNode;
            LSelectedNode := LCurrentNode;
            Break;
          end;
        end;
      end;

      if (not Assigned(LSelectedParentNode)) or (not Assigned(LSelectedNode)) then
      begin
        if FTreeViewNodes.Count > 0 then
        begin
          LSelectedParentNode := FTreeView.Items[0];
          LSelectedNode := FTreeView.Items[0];
        end;
      end;

      // Try and find the required child node.
      if (AViewDataNode.ViewDataSetCount > 0) then
//         (AViewDataNode.ViewDataSet.ParamCount > 0) then
      begin
        if Assigned(LSelectedParentNode) then
        begin
          for LCount := 0 to LSelectedParentNode.Count - 1 do
          begin
            LCurrentNode := LSelectedParentNode[LCount];
            LDataObject := TViewDataTreeNodeData(LCurrentNode.Data);
            if Assigned(LDataObject) and
               Assigned(LDataObject.ViewDataNode) then
  //             Assigned(LDataObject.ViewDataNode.ViewDataSet) then
            begin
  //            if (LDataObject.ViewDataNode.ViewDataSet.IDValues = AViewDataNode.IDValues) then
              if (LDataObject.ViewDataNode.IsViewDataSetPresent(AViewDataNode.IDValues)) then
              begin
                LSelectedNode := LCurrentNode;
                Break;
              end;
            end;
          end;
        end;
      end;

      // Check that the node was found.
      if not Assigned(LSelectedNode) then
      begin
        raise Exception.Create('The selected dataset node does not exist. ID = ' + AViewDataNode.ViewID);
      end else begin
        FTreeView.Selected := LSelectedNode;
        Result := True;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDataViewerSheet.DoGridAction(AGridActionObject: TGridActionObject): boolean;
const OPNAME = 'TDataViewerSheet.DoGridAction';
var
  LIndex: integer;
  LCanChange: boolean;
begin
  Result := False;
  try
    for LIndex := 0 to FTreeViewNodes.Count - 1 do
    begin
      if Assigned(TTreeNode(FTreeViewNodes.Objects[LIndex]).Data) then
      begin
        if (TViewDataTreeNodeData(TTreeNode(FTreeViewNodes.Objects[LIndex]).Data).ViewDataNode.ViewID = AGridActionObject.DatasetID) then
        begin
          LCanChange := True;
          DoTreeNodeAboutToChange(nil, nil, LCanChange);
          if LCanChange then
          begin
            FTreeView.Selected := TTreeNode(FTreeViewNodes.Objects[LIndex]);
            DoTreeNodeHasChanged(nil, nil);
            Result := True;
            break;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataViewerSheet.DoTreeNodeAboutToChange(ASender: TObject; ANode: TTreeNode; var AAllowChange: Boolean);
const OPNAME = 'TDataViewerSheet.DoTreeNodeAboutToChange';
begin
  try
    if (not (csDestroying in ComponentState)) then
    begin
      if AAllowChange then
      begin
        if Assigned(FCurrentDataset) and Assigned(FCurrentDataset.DataSet()) then
          FCurrentDataset.DataSet.Close;
        ClearDataViewer;
        if Assigned(FAppModules.Model()) then
          FAppModules.Model.ViewData.CurrentTreeNodeData := nil;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataViewerSheet.DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode);
const OPNAME = 'TDataViewerSheet.DoTreeNodeHasChanged';
begin
  try
    if (not (csDestroying in ComponentState)) then
    begin
      if Assigned(FTreeView.Selected) then
      begin
        if Assigned(FTreeView.Selected.Data) then
        begin
          PopulateDataViewer(TViewDataTreeNodeData(FTreeView.Selected.Data));
          if Assigned(FAppModules.Model()) then
            FAppModules.Model.ViewData.CurrentTreeNodeData := FTreeView.Selected.Data;
        end;
      end;
      if Assigned(FAppModules) and Assigned(FAppModules.Model()) then
        FAppModules.Model.ProcessEvent(CmeRefreshMenuItems, nil);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataViewerSheet.ClearDataViewer;
const OPNAME = 'TDataViewerSheet.ClearDataViewer';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDataViewerSheet.PopulateDataViewer(ADataObject: TViewDataTreeNodeData);
const OPNAME = 'TDataViewerSheet.PopulateDataViewer';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

