//
//
//  UNIT      : Contains the class TViewDataList.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UViewDataList;

interface

uses
  Contnrs,
  Classes,
  vcl.ComCtrls,
  VoaimsCom_TLB,
  UViewDataItem,
  UViewModelDataObject,
  UAbstractObject;

type
  TViewDataList = class(TAbstractViewDataList)
  protected
    FLastSumOutIndex: integer;
    FSequence : integer;
    FLoadCase : integer;
    FLastSumOutCaption: string;
    FIndexCanChange: boolean;
    FParentNodesIds: TStringList;
    FCurrentTreeNodeData: TViewDataTreeNodeData;
    FViewDataSets: TObjectList;
    FViewDataNodes: TObjectList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure LoadViewDataSets(AViewDataDataSet: TAbstractModelDataset);
    procedure PopulateViewDataSetItem(AViewDataSetItem: TViewDataSet; AViewDataDataSet: TAbstractModelDataset);
    procedure LoadTreeNodeStructure(AViewDataNodesDataSet: TAbstractModelDataset);
    procedure PopulateViewDataNode(AViewDataNode: TViewDataNode; AViewDataNodesDataSet: TAbstractModelDataset);
    function GetParentsParent(AParentID: string; var AParentsParent: string): boolean;
    procedure LoadJumps(AJumpsData: TStringList; AViewDataJumpsDataSet: TAbstractModelDataset);
    procedure PopulateViewDataJumps(AViewDataItem: TViewDataNode; AJumpsData: TStringList);
    procedure LoadSubNodes(AViewDataSubNodesDataSet: TAbstractModelDataset);
    procedure LoadSubNodesFromFile(ASubNodesFromFile : TStrings;ASubNodesDatasetID : string);
    procedure LoadSubNodeDataSets(ASubNodeDataSets: TStringList);
    procedure AddDynamicSubNodeFromFile(AMasterNodeData: TViewDataNode;ASubNodeFromFile : string);
    procedure AddDynamicSubNode(ASubNodeIndex: integer; AMasterNodeData: TViewDataNode;
      AViewDataSubNodesDataSet: TAbstractModelDataset;AViewModelDataItem: TViewModelDataItem);
    procedure ClearTreeNodes(ATreeNodes: TTreeNodes);
    procedure PopulateParentTreeNodes(ATreeNodes: TTreeNodes; ATabsheetID: string);
    procedure PopulateChildrenTreeNodes(ATreeNodes: TTreeNodes; ATabsheetID: string);
    function CanSubNodeBeAddedToTree(AViewDataNode: TViewDataNode; AParentTreeNode: TTreeNode;ATabsheetID: string): boolean;
    procedure SetImageIndexes(ATreeNodes: TTreeNodes);
    function GetViewDataSet(ADatasetID: string): TViewDataSet;
    function GetViewDataNode(AViewID: string): TViewDataNode;
    function GetCurrentTreeNodeData: TObject; override;
    procedure SetCurrentTreeNodeData(ATreeNodeData: TObject); override;
    function FindParentTreeNode(ATreeNodes: TTreeNodes; AViewID: string): TTreeNode;
    function Get_ViewDataNodeCount: integer;
    function Get_ViewDataNodeByIndex(AIndex: integer): TViewDataNode;
  public
    procedure Clear;
    procedure Populate(AViewDataDataSet, AViewDataSubNodesDataSet, AViewDataTreeNodesDataSet, AViewDataJumpsDataSet: TAbstractModelDataset);
    procedure PopulateJumpList(AJumpList: TStringList);
    procedure PopulateTreeNodes(ATreeNodes: TObject; ATabsheetID: string); override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function AddViewDataNode (AViewDataNode : TViewDataNode) : boolean;
    function DeleteViewDataNode (AViewDataNode : TViewDataNode) : boolean;
    property ViewDataSet[ADatasetID: string]: TViewDataSet read GetViewDataSet;
    property ViewDataNode[AViewID: string]: TViewDataNode read GetViewDataNode;
    property ViewDataNodeCount: integer read Get_ViewDataNodeCount;
    property ViewDataNodeByIndex[AIndex: integer]: TViewDataNode read Get_ViewDataNodeByIndex;
  end;

implementation

uses
  SysUtils,
  Data.DB,
  UConstants,
  UUtilities,
  System.Types,
  //UFunctionTimer,
  UAbstractModelData,
  //UYieldModelDataObject,
  //UOutputData,
  UAbstractComponent,
  UStringFieldOperations,
  UErrorHandlingOperations;

procedure TViewDataList.CreateMemberObjects;
const OPNAME = 'TViewDataList.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FViewDataSets := TObjectList.Create;
    FViewDataNodes := TObjectList.Create;
    FParentNodesIds := TStringList.Create;
    FParentNodesIds.Sorted := True;
    FParentNodesIds.Duplicates := dupIgnore;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.DestroyMemberObjects;
const OPNAME = 'TViewDataList.DestroyMemberObjects';
begin
  try
    Clear;
    FreeAndNil(FViewDataNodes);
    FreeAndNil(FViewDataSets);
    FreeAndNil(FParentNodesIds);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.Clear;
const OPNAME = 'TViewDataList.Clear';
begin
  try
    FLastSumOutIndex   := 0;
    FSequence          := 0;
    FLoadCase          := 0;
    FLastSumOutCaption := '';
    FIndexCanChange    := True;
    FViewDataNodes.Clear;
    FViewDataSets.Clear;
    FParentNodesIds.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.GetViewDataSet(ADatasetID: string): TViewDataSet;
const OPNAME = 'TViewDataList.GetViewDataSet';
var LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FViewDataSets.Count - 1 do
    begin
      if (TViewDataSet(FViewDataSets[LIndex]).DatasetID = ADatasetID) then
      begin
        Result := TViewDataSet(FViewDataSets[LIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.GetViewDataNode(AViewID: string): TViewDataNode;
const OPNAME = 'TViewDataList.GetViewDataNode';
var LIndex: integer;
begin
  Result := nil;
  try
    for LIndex := 0 to FViewDataNodes.Count - 1 do
    begin
      if (TViewDataNode(FViewDataNodes[LIndex]).ViewID = AViewID) then
      begin
        Result := TViewDataNode(FViewDataNodes[LIndex]);
        break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.AddViewDataNode (AViewDataNode : TViewDataNode) : boolean;
const OPNAME = 'TViewDataList.AddViewDataNode';
var
  lIndex    : integer;
  lFound    : boolean;
  lParentID : string;
  lViewID   : string;
  lViewNode : TViewDataNode;
begin
  Result := FALSE;
  try
    lParentID := AViewDataNode.ParentID;
    lViewID   := AViewDataNode.ViewID;
    lIndex := 0;
    lFound := FALSE;
    while ((NOT lFound) AND (lIndex < FViewDataNodes.Count)) do
    begin
      if ((TViewDataNode(FViewDataNodes[lIndex]).ParentID = lParentID) AND
          (TViewDataNode(FViewDataNodes[lIndex]).ViewID = lViewID)) then
        lFound := TRUE
      else
        lIndex := lIndex + 1;
    end;
    if (lFound) then
    begin
      lFound := FALSE;
      while ((NOT lFound) AND (lIndex < FViewDataNodes.Count)) do
      begin
        lViewNode := TViewDataNode(FViewDataNodes[lIndex]);
        if (lViewNode.ParentID <> lParentID) then
          lFound := TRUE
        else
        begin
          if (lViewNode.DataType = 'CHANNEL') then
          begin
            if (lViewNode.Weighting > AViewDataNode.Weighting) then
              lFound := TRUE
            else
              lIndex := lIndex + 1;
          end
          else
          begin
            if (lViewNode.OverrideCaption > AViewDataNode.OverrideCaption) then
              lFound := TRUE
            else
              lIndex    := lIndex + 1;
          end;
        end;
      end;
      FViewDataNodes.Insert(lIndex, AViewDataNode);
      Result := TRUE;
    end
    else
    begin
      FViewDataNodes.Add(AViewDataNode);
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.DeleteViewDataNode (AViewDataNode : TViewDataNode) : boolean;
const OPNAME = 'TViewDataList.DeleteViewDataNode';
begin
  Result := FALSE;
  try
    if (FViewDataNodes.Remove(AViewDataNode) >= 0) then
      Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.Populate(AViewDataDataSet, AViewDataSubNodesDataSet, AViewDataTreeNodesDataSet, AViewDataJumpsDataSet: TAbstractModelDataset);
const OPNAME = 'TViewDataList.Populate';
var
  LDataSetID, LJumpData: TStringList;
  LIndex, LDataSetIndex: integer;
begin
  try

    // Clear all the existing data.
    Clear;
    LJumpData := TStringList.Create;
    try

      // Load the tables.
      LoadViewDataSets(AViewDataDataSet);
      LoadTreeNodeStructure(AViewDataTreeNodesDataSet);
      LoadJumps(LJumpData, AViewDataJumpsDataSet);

      // Go through the list and add the SQL references.
      LDataSetID := TStringList.Create;
      try
        for LIndex := 0 to FViewDataNodes.Count - 1 do
        begin
          LDataSetID.CommaText := TViewDataNode(FViewDataNodes[LIndex]).DatasetIDCommaText;
          for LDataSetIndex := 0 to LDataSetID.Count - 1 do
            TViewDataNode(FViewDataNodes[LIndex]).AddViewDataset(
              ViewDataSet[LDataSetID[LDataSetIndex]]);
          TViewDataNode(FViewDataNodes[LIndex]).SubNodesDataSet :=
            ViewDataSet[TViewDataNode(FViewDataNodes[LIndex]).SubNodesDatasetID];
        end;
      finally
        LDataSetID.Free;
      end;

      // Go through the list and add all of the jump references.
      for LIndex := 0 to FViewDataNodes.Count - 1 do
        PopulateViewDataJumps(TViewDataNode(FViewDataNodes[LIndex]), LJumpData);

      // Load the sub node data after the jumps have been loaded.
      LoadSubNodes(AViewDataSubNodesDataSet);
    finally
      LJumpData.Free;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.LoadViewDataSets(AViewDataDataSet: TAbstractModelDataset);
const OPNAME = 'TViewDataList.LoadViewDataSets';
var LViewDataSet: TViewDataSet;
begin
  try

    // Open and run through the dataset.
    if Assigned(AViewDataDataSet) and Assigned(AViewDataDataSet.DataSet()) then
    begin
      AViewDataDataSet.DataSet.Open;
      try
        while (not AViewDataDataSet.DataSet.EOF) do
        begin
          LViewDataSet := TViewDataSet.Create;
          PopulateViewDataSetItem(LViewDataSet, AViewDataDataSet);
          FViewDataSets.Add(LViewDataSet);
          AViewDataDataSet.DataSet.Next;
        end;
      finally
        AViewDataDataSet.DataSet.Close;
      end;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.PopulateViewDataSetItem(AViewDataSetItem: TViewDataSet; AViewDataDataSet: TAbstractModelDataset);
const OPNAME = 'TViewDataList.PopulateViewDataSetItem';
var
  LEditable: Boolean;
begin
  try
    LEditable := AViewDataDataSet.DataSet.FieldByName('Editable').AsBoolean;
    LEditable := LEditable and (FAppModules.User.UserRights in CUR_EditData);
    LEditable := LEditable and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    AViewDataSetItem.DatasetID     := Trim(AViewDataDataSet.DataSet.FieldByName('DatasetID').AsString);
    AViewDataSetItem.Editable      := LEditable;
    AViewDataSetItem.SQLType       := AViewDataDataSet.DataSet.FieldByName('SQLType').AsInteger;
    AViewDataSetItem.NoDataMessage := Trim(AViewDataDataSet.DataSet.FieldByName('NoDataMessage').AsString);
    AViewDataSetItem.ViewSQL       := Trim(AViewDataDataSet.DataSet.FieldByName('ViewSQL').AsString);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.LoadTreeNodeStructure(AViewDataNodesDataSet: TAbstractModelDataset);
const OPNAME = 'TViewDataList.LoadTreeNodeStructure';
var
  LViewDataNode: TViewDataNode;
  LIndex: integer;
  LParentsParent: string;
begin
  try

    // Open and run through the dataset.
    if Assigned(AViewDataNodesDataSet) and Assigned(AViewDataNodesDataSet.DataSet()) then
    begin
      AViewDataNodesDataSet.DataSet.Open;
      try
        while (not AViewDataNodesDataSet.DataSet.EOF) do
        begin
          LViewDataNode := TViewDataNode.Create;
          PopulateViewDataNode(LViewDataNode, AViewDataNodesDataSet);
          FViewDataNodes.Add(LViewDataNode);
          AViewDataNodesDataSet.DataSet.Next;
        end;
      finally
        AViewDataNodesDataSet.DataSet.Close;
      end;
    end;

    // Populate the top parent ID field after all nodes are added.
    for LIndex := 0 to FViewDataNodes.Count - 1 do
    begin

      // Recurse back through all parent's parents.
      LParentsParent := TViewDataNode(FViewDataNodes[LIndex]).ParentID;
      while GetParentsParent(LParentsParent, LParentsParent) do ;

      // Set the value to the last valid parent's parent.
      TViewDataNode(FViewDataNodes[LIndex]).TopParentID := LParentsParent;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.GetParentsParent(AParentID: string; var AParentsParent: string): boolean;
const OPNAME = 'TViewDataList.GetParentsParent';
var LIndex: integer;
begin
  Result := False;
  AParentsParent := '';
  try
    for LIndex := 0 to FViewDataNodes.Count - 1 do
    begin
      if (TViewDataNode(FViewDataNodes[LIndex]).ViewID = AParentID) then
      begin
        AParentsParent := TViewDataNode(FViewDataNodes[LIndex]).ParentID;
        Result := True;
        break;
      end;
    end;

    // Set the parent's parent to Parent if the view ID was not found.
    if (not Result) then
      AParentsParent := AParentID;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.PopulateViewDataNode(AViewDataNode: TViewDataNode; AViewDataNodesDataSet: TAbstractModelDataset);
const OPNAME = 'TViewDataList.PopulateViewDataNode';
begin
  try
    AViewDataNode.ViewID             := Trim(AViewDataNodesDataSet.DataSet.FieldByName('ViewID').AsString);
    AViewDataNode.ParentID           := Trim(AViewDataNodesDataSet.DataSet.FieldByName('ParentID').AsString);
    AViewDataNode.TopParentID        := '';
    AViewDataNode.Weighting          := AViewDataNodesDataSet.DataSet.FieldByName('Weighting').AsInteger;
    AViewDataNode.OverrideCaption    := '';
    AViewDataNode.ShowSQL            := (Trim(AViewDataNodesDataSet.DataSet.FieldByName('ShowSQL').AsString) <> '0');
    AViewDataNode.BitmapName         := Trim(AViewDataNodesDataSet.DataSet.FieldByName('BitmapName').AsString);
    AViewDataNode.DatasetIDCommaText := Trim(AViewDataNodesDataSet.DataSet.FieldByName('DatasetID').AsString);
    AViewDataNode.SubNodesDatasetID  := Trim(AViewDataNodesDataSet.DataSet.FieldByName('SubNodesDatasetID').AsString);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.LoadJumps(AJumpsData: TStringList; AViewDataJumpsDataSet: TAbstractModelDataset);
const OPNAME = 'TViewDataList.LoadJumps';
begin
  try

    // Open and run through the dataset.
    AJumpsData.Clear;
    if Assigned(AViewDataJumpsDataSet) and Assigned(AViewDataJumpsDataSet.DataSet()) then
    begin
      AViewDataJumpsDataSet.DataSet.Open;
      try
        while (not AViewDataJumpsDataSet.DataSet.EOF) do
        begin
          AJumpsData.Add(
            Trim(AViewDataJumpsDataSet.DataSet.FieldByName('ViewID').AsString) + ',' +
            Trim(AViewDataJumpsDataSet.DataSet.FieldByName('JumpToID').AsString));
          AViewDataJumpsDataSet.DataSet.Next;
        end;
      finally
        AViewDataJumpsDataSet.DataSet.Close;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.LoadSubNodes(AViewDataSubNodesDataSet: TAbstractModelDataset);
const OPNAME = 'TViewDataList.LoadSubNodes';
var
  LHandled: boolean;
  LSubNodesDataSetIndex, LMasterNodeIndex, LSubNodeIndex,LDataCount: integer;
  LMasterNodeData: TViewDataNode;
  LSubNodeDataSets: TStringList;
  LSubNodeFromFile : TStringList;
  LIndex : integer;
  LDataItemsList: TViewModelDataItemsList;
  LViewModelDataItem: TViewModelDataItem;
begin
  try

    // Make sure that a data set is available.
    if Assigned(AViewDataSubNodesDataSet) and Assigned(AViewDataSubNodesDataSet.DataSet()) then
    begin

      // Get all the sub node data sets being used.
      LSubNodeDataSets   := TStringList.Create;
      LDataItemsList     := TViewModelDataItemsList.Create;
      try
        LoadSubNodeDataSets(LSubNodeDataSets);

        // Process every sub node data set found.
        for LSubNodesDataSetIndex := 0 to LSubNodeDataSets.Count - 1 do
        begin
          LHandled := False;
          LDataItemsList.Reset;
          TAbstractModelData(FAppModules.Model.ModelData).GetViewDataItems(LSubNodeDataSets[LSubNodesDataSetIndex],LDataItemsList,LHandled);
          FLastSumOutIndex   := 0;
          FSequence          := 0;
          FLoadCase          := 0;
          FLastSumOutCaption := '';
          FIndexCanChange    := True;

          if LHandled then
          begin
            LSubNodeIndex := 0;
            for LDataCount := 0 to LDataItemsList.ItemsCount -1 do
            begin
              LViewModelDataItem := LDataItemsList.ViewModelDataItemByIndex[LDataCount];
              if Assigned(LViewModelDataItem) then
              begin
                FIndexCanChange    := True;
                for LMasterNodeIndex := 1 to TList(LSubNodeDataSets.Objects[LSubNodesDataSetIndex]).Count - 1 do
                begin
                  LMasterNodeData := TList(LSubNodeDataSets.Objects[LSubNodesDataSetIndex])[LMasterNodeIndex];
                  AddDynamicSubNode(LSubNodeIndex, LMasterNodeData, nil,LViewModelDataItem);
                  FIndexCanChange    := False;
                end;
              end;
              Inc(LSubNodeIndex);
            end;
          end
          else
          begin
            LSubNodeIndex := 0;
            AViewDataSubNodesDataSet.ClearSQL;
            AViewDataSubNodesDataSet.SetSQL(
              TViewDataSet(TList(LSubNodeDataSets.Objects[LSubNodesDataSetIndex])[0]).ViewSQL);
            FAppModules.StudyArea.SetDefaultParams(AViewDataSubNodesDataSet);
            AViewDataSubNodesDataSet.DataSet.Open;
            try
              while (not AViewDataSubNodesDataSet.DataSet.EOF) do
              begin
                FIndexCanChange    := True;

                for LMasterNodeIndex := 1 to TList(LSubNodeDataSets.Objects[LSubNodesDataSetIndex]).Count - 1 do
                begin
                  LMasterNodeData := TList(LSubNodeDataSets.Objects[LSubNodesDataSetIndex])[LMasterNodeIndex];
                  AddDynamicSubNode(LSubNodeIndex, LMasterNodeData, AViewDataSubNodesDataSet,nil);
                  FIndexCanChange    := False;
                end;

                AViewDataSubNodesDataSet.DataSet.Next;
                Inc(LSubNodeIndex);
              end;
              if AViewDataSubNodesDataSet.DataSet.IsEmpty then
              begin
                LSubNodeFromFile := TStringList.Create;
                try
                  LSubNodeFromFile.Sorted := True;
                  LSubNodeFromFile.Duplicates := dupAccept;
                  LoadSubNodesFromFile(LSubNodeFromFile,TViewDataSet(TList(LSubNodeDataSets.Objects[LSubNodesDataSetIndex])[0]).DatasetID);
                  for LIndex := 0 to LSubNodeFromFile.Count-1 do
                  begin
                    FIndexCanChange    := True;
                    for LMasterNodeIndex := 1 to TList(LSubNodeDataSets.Objects[LSubNodesDataSetIndex]).Count - 1 do
                    begin
                      LMasterNodeData := TList(LSubNodeDataSets.Objects[LSubNodesDataSetIndex])[LMasterNodeIndex];
                      if (Trim(LMasterNodeData.ParentID) = 'SumOutTimeSeries') or
                         (Trim(LMasterNodeData.ParentID) = 'SumOutHeading') or
                         (Trim(LMasterNodeData.ParentID) = 'SumOutGraph') then
                        AddDynamicSubNodeFromFile(LMasterNodeData,LSubNodeFromFile[LIndex]);
                      FIndexCanChange    := False;
                    end;
                  end;
                finally
                  FreeAndNil(LSubNodeFromFile);
                end;
              end;
            finally
              AViewDataSubNodesDataSet.DataSet.Close;
            end;
          end;
        end;
      finally
        LDataItemsList.Free;
        LSubNodeDataSets.Free;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.LoadSubNodeDataSets(ASubNodeDataSets: TStringList);
const OPNAME = 'TViewDataList.LoadSubNodeDataSets';
var
  LMasterNodeIndexOuter, LMasterNodeIndexInner: integer;
  LMasterNodeDataOuter, LMasterNodeDataInner: TViewDataNode;
  LList: TList;
begin
  try

    // Load all the unique data set ID's
    ASubNodeDataSets.Clear;
    ASubNodeDataSets.Sorted := True;
    for LMasterNodeIndexOuter := 0 to FViewDataNodes.Count - 1 do
    begin
      LMasterNodeDataOuter := TViewDataNode(FViewDataNodes[LMasterNodeIndexOuter]);
      if (LMasterNodeDataOuter.ViewDataSetCount > 0) and
         Assigned(LMasterNodeDataOuter.SubNodesDataSet) and
         (LMasterNodeDataOuter.SubNodesDataSet.ViewSQL <> '') then
      begin
        if (ASubNodeDataSets.IndexOf(LMasterNodeDataOuter.SubNodesDataSet.DatasetID) < 0) then
        begin
          LList := TList.Create;

          // The first elements is the data set object and this node object.
          LList.Add(LMasterNodeDataOuter.SubNodesDataSet);
          LList.Add(LMasterNodeDataOuter);

          // The other elements are all the nodes that also use this data set.
          for LMasterNodeIndexInner := LMasterNodeIndexOuter + 1 to FViewDataNodes.Count - 1 do
          begin
            LMasterNodeDataInner := TViewDataNode(FViewDataNodes[LMasterNodeIndexInner]);
            if (LMasterNodeDataInner.ViewDataSetCount > 0) and
               Assigned(LMasterNodeDataInner.SubNodesDataSet) and
               (LMasterNodeDataInner.SubNodesDataSet.DatasetID = LMasterNodeDataOuter.SubNodesDataSet.DatasetID) and
               (LMasterNodeDataInner.SubNodesDataSet.ViewSQL <> '') then
            begin
              LList.Add(LMasterNodeDataInner);
            end;
          end;

          // Add the ID and the list to the string list.
          ASubNodeDataSets.AddObject(LMasterNodeDataOuter.SubNodesDataSet.DatasetID, LList);
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.LoadSubNodesFromFile(ASubNodesFromFile : TStrings;ASubNodesDatasetID : string);
const OPNAME = 'TViewDataList.LoadSubNodesFromFile';
var
  LOutputData: IOutputData;
  LElementDataType:TOutputDataType;
  LSubNodesFromFile : wideString;
begin
  try
    LElementDataType := GetElementDataTypeBySubNodesDatasetID(ASubNodesDatasetID);
    if(LElementDataType <> btNone) then
    begin
      LOutputData := (FAppModules.Model.ModelData as IYieldModelData).OutputData;
      if LOutputData <> nil then
      begin
      LOutputData.SummaryOutputData.GetElementsByElementDataType(LElementDataType,LSubNodesFromFile);
      ASubNodesFromFile.CommaText := LSubNodesFromFile;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.AddDynamicSubNodeFromFile(AMasterNodeData: TViewDataNode;ASubNodeFromFile : string);
const OPNAME = 'TViewDataList.AddDynamicSubNodeFromFile';
var
  LSubNodeData : TViewDataNode;
  LIndex : integer;
  LSubNodeViewDataSet: TViewDataSet;
  LSubNodeFromFile : TStringList;
  LElementID : string;
  LRun : IRunConfigurationData;
begin
  try
    LSubNodeFromFile := TStringList.Create;
    LRun := (FAppModules.Model.ModelData as IYieldModelData).RunConfigurationData;
    try
      LSubNodeData := TViewDataNode.Create;
      LSubNodeData.AssignFrom(AMasterNodeData);
      LSubNodeData.ViewID := AMasterNodeData.ViewID;
      LSubNodeData.ParentID := AMasterNodeData.ViewID;
      LSubNodeData.Weighting := AMasterNodeData.Weighting-1;
      LSubNodeData.DataType := '';
      LSubNodeData.ShowSQL := True;
      LSubNodeData.DatasetIDCommaText := '';
      LSubNodeData.ClearViewDatasets;
      LSubNodeData.SubNodesDataSet := nil;
      LSubNodeFromFile.CommaText :=ASubNodeFromFile;
      LSubNodeData.OverrideCaption := Trim(Copy(ASubNodeFromFile,1,Pos(',',ASubNodeFromFile)-1));
      LElementID := Trim(Copy(ASubNodeFromFile,Pos(',',ASubNodeFromFile)+1,Length(ASubNodeFromFile)));
      if (LSubNodeFromFile.Count-1>=0) then
        LElementID := LSubNodeFromFile[LSubNodeFromFile.Count-1];

      if Trim(LElementID) <> '' then
        LSubNodeData.ElementID := StrToInt(LElementID);

      if(Pos('SumOutYearFile',LSubNodeData.ViewID) = 1) then
      begin
        if(LSubNodeData.OverrideCaption <> FLastSumOutCaption) then
        begin
          FLastSumOutCaption := LSubNodeData.OverrideCaption;
          FLastSumOutIndex   := 1;
          if LRun <> nil then
          begin
            if LRun.RunSequenceType = 'S' then
            begin
              FSequence := 1;
              FLoadCase := 1;
            end;
          end;
        end
        else
        begin
          if FIndexCanChange then
          begin
            FLastSumOutIndex   := FLastSumOutIndex + 1;
            if LRun <> nil then
            begin
              if LRun.RunSequenceType = 'S' then
              begin
                FSequence := FSequence + 1;
                if (FSequence > LRun.NumberOfSequencesInAnalysis) then
                begin
                  FSequence := 1;
                  FLoadCase := FLoadCase + 1;
                end;

              end;
            end;
          end;
        end;

        if LRun <> nil then
        begin
          if LRun.RunSequenceType = 'S' then
            LSubNodeData.OverrideCaption := LSubNodeData.OverrideCaption + ' (Sequence '+IntToStr(FSequence)+') (Load Case ' + IntToStr(FLoadCase)+ ')'
          else
            LSubNodeData.OverrideCaption := LSubNodeData.OverrideCaption + ' (Load Case ' + IntToStr(FLastSumOutIndex)+ ')';
        end;
        
      end;

      if LRun.RunSequenceType <> 'S' then
        LSubNodeData.LoadCase := FLastSumOutIndex
      else
      begin
        LSubNodeData.LoadCase := FLoadCase;
        LSubNodeData.Sequence := FSequence;
      end;

      for LIndex := 0 to AMasterNodeData.ViewDataSetCount -1 do
      begin
        LSubNodeViewDataSet := TViewDataSet.Create;
        LSubNodeViewDataSet.AssignFrom(AMasterNodeData.ViewDataSet[LIndex]);
        LSubNodeViewDataSet.DatasetID := AMasterNodeData.ViewDataSet[LIndex].DatasetID;
        LSubNodeViewDataSet.LoadFromFile := True;

        LSubNodeData.AddViewDataset(LSubNodeViewDataSet);
        FViewDataSets.Add(LSubNodeViewDataSet);
      end;
      FViewDataNodes.Add(LSubNodeData);
    finally
      FreeAndNil(LSubNodeFromFile);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.AddDynamicSubNode(ASubNodeIndex: integer; AMasterNodeData: TViewDataNode;
          AViewDataSubNodesDataSet: TAbstractModelDataset;AViewModelDataItem: TViewModelDataItem);
const OPNAME = 'TViewDataList.AddDynamicSubNode';
var
  LIndex, LDatasetIndex: integer;
  LSubNodeData: TViewDataNode;
  LSubNodeViewSQL: TViewDataSet;
  LParamNames, LParamValues: TStringList;
begin
  try

    // Create the local objects.
    LParamNames := TStringList.Create;
    try
      LParamValues := TStringList.Create;
      try

        // Create the sub node data object.
        LSubNodeData := TViewDataNode.Create;
        LSubNodeData.AssignFrom(AMasterNodeData);
        LSubNodeData.ViewID := AMasterNodeData.ViewID;
        LSubNodeData.ParentID := AMasterNodeData.ViewID;


        if Assigned(AViewModelDataItem) then
          LSubNodeData.Weighting := AViewModelDataItem.Weighting
        else
          LSubNodeData.Weighting := AViewDataSubNodesDataSet.DataSet.FieldByName('Weighting').AsInteger;

        if Assigned(AViewModelDataItem) then
          LSubNodeData.OverrideCaption := AViewModelDataItem.Caption
        else
        LSubNodeData.OverrideCaption := Trim(AViewDataSubNodesDataSet.DataSet.FieldByName('Caption').AsString);

        if Assigned(AViewModelDataItem) then
          LSubNodeData.DataType := AViewModelDataItem.DataType
        else
          LSubNodeData.DataType := '';

        if(Pos('SumOutYearFile',LSubNodeData.ViewID) = 1) then
        begin
          if(LSubNodeData.OverrideCaption <> FLastSumOutCaption) then
          begin
            FLastSumOutCaption := LSubNodeData.OverrideCaption;
            FLastSumOutIndex   := 1;
          end
          else
          begin
            if FIndexCanChange then
              FLastSumOutIndex   := FLastSumOutIndex + 1;
          end;
          LSubNodeData.OverrideCaption := LSubNodeData.OverrideCaption + ' (Load Case ' + IntToStr(FLastSumOutIndex)+ ')';
        end;

        LSubNodeData.ShowSQL := True;
        LSubNodeData.DatasetIDCommaText := '';
        LSubNodeData.DatasetIDCommaText := '';
        LSubNodeData.ClearViewDatasets;
        LSubNodeData.SubNodesDataSet := nil;

        // Create the sub node view SQL object.
        for LDatasetIndex := 0 to AMasterNodeData.ViewDataSetCount -1 do
        begin
          LSubNodeViewSQL := TViewDataSet.Create;
          LSubNodeViewSQL.AssignFrom(AMasterNodeData.ViewDataSet[LDatasetIndex]);
          LSubNodeViewSQL.DatasetID := AMasterNodeData.ViewDataSet[LDatasetIndex].DatasetID;

          if Assigned(AViewModelDataItem) then
            ExtractFields(AViewModelDataItem.ParamNames,',', LParamNames)
          else
            ExtractFields(Trim(AViewDataSubNodesDataSet.DataSet.FieldByName('ParamNames').AsString), ',', LParamNames);

          if Assigned(AViewModelDataItem) then
            ExtractFields(AViewModelDataItem.ParamValues,',', LParamValues)
          else
          ExtractFields(Trim(AViewDataSubNodesDataSet.DataSet.FieldByName('ParamValues').AsString), ',', LParamValues);

          LSubNodeViewSQL.LoadParams(LParamNames, LParamValues);
          LSubNodeData.AddViewDataset(LSubNodeViewSQL);

          // Set the param values for the master node.
          for LIndex := 0 to LParamValues.Count - 1 do
            LParamValues[LIndex] := '%';
          AMasterNodeData.ViewDataSet[LDatasetIndex].LoadParams(LParamNames, LParamValues);

          // Add the two new objects.
          FViewDataSets.Add(LSubNodeViewSQL);
        end;

        // Add the two new objects.
        FViewDataNodes.Add(LSubNodeData);

      // Clean up.
      finally
        LParamValues.Free;
      end;
    finally
      LParamNames.Free;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;



procedure TViewDataList.PopulateViewDataJumps(AViewDataItem: TViewDataNode; AJumpsData: TStringList);
const OPNAME = 'TViewDataList.PopulateViewDataJumps';
var
  LPosOfComma: integer;
  LJumpDataIndex: integer;
  LJumpFromDatasetID: string;
  LJumpToDatasetID: string;
  LJumpToDataItem: TViewDataNode;
begin
  try

    // Loop through all the jumps for this ViewID.
    for LJumpDataIndex := 0 to AJumpsData.Count - 1 do
    begin
      LPosOfComma := Pos(',', AJumpsData[LJumpDataIndex]);
      LJumpFromDatasetID := Copy(AJumpsData[LJumpDataIndex], 1, LPosOfComma - 1);
      if (LJumpFromDatasetID = AViewDataItem.ViewID) then
      begin
        LJumpToDatasetID := Copy(AJumpsData[LJumpDataIndex], LPosOfComma + 1, Length(AJumpsData[LJumpDataIndex]));
        LJumpToDataItem := ViewDataNode[LJumpToDatasetID];
        if Assigned(LJumpToDataItem) then
        begin
          AViewDataItem.AddJump(LJumpToDataItem);
        end else begin
          ReportError(Format('Unknown JumpTo Dataset ID [%s]', [LJumpToDatasetID]), OPNAME);
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.PopulateTreeNodes(ATreeNodes: TObject; ATabsheetID: string);
const OPNAME = 'TViewDataList.PopulateTreeNodes';
var
  LNodes:TTreeNodes;
begin
  try
    if Assigned(ATreeNodes) then
    begin
      LNodes := TTreeNodes(ATreeNodes);
      LNodes.BeginUpdate;
      try
        ClearTreeNodes(LNodes);
        PopulateParentTreeNodes(LNodes, ATabsheetID);
        PopulateChildrenTreeNodes(LNodes, ATabsheetID);
        SetImageIndexes(LNodes);
      finally
        LNodes.EndUpdate;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.ClearTreeNodes(ATreeNodes: TTreeNodes);
const OPNAME = 'TViewDataList.ClearTreeNodes';
var LIndex: integer;
begin
  try
    for LIndex := 0 to ATreeNodes.Count - 1 do
    begin
      TObject(ATreeNodes.Item[LIndex].Data).Free;
      ATreeNodes.Item[LIndex].Data := nil;
    end;
    ATreeNodes.Clear;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.PopulateParentTreeNodes(ATreeNodes: TTreeNodes; ATabsheetID: string);
const OPNAME = 'TViewDataList.PopulateParentTreeNodes';
var
  LViewDataNodesIndex: integer;
  LCurrentViewDataNode: TViewDataNode;
  LViewDataTreeNodeData: TViewDataTreeNodeData;
  LSQLType: string;
begin
  try
     FParentNodesIds.Clear;

    // Add the parent nodes.
    for LViewDataNodesIndex := 0 to FViewDataNodes.Count - 1 do
    begin

      // Check if this node is the correct type.
      LCurrentViewDataNode := TViewDataNode(FViewDataNodes[LViewDataNodesIndex]);
      if(FAppModules.StudyArea.ModelVersion <> '7') then
      begin
        if((UpperCase(LCurrentViewDataNode.ViewID) = 'IRRIGATIONBLOCK')) then Continue;
        if((UpperCase(LCurrentViewDataNode.ViewID) = 'WETLAND')) then Continue;
        if((UpperCase(LCurrentViewDataNode.ViewID) = 'REVIEWWETLAND')) then Continue;
      end;

      if (LCurrentViewDataNode.ParentID = Copy(ATabsheetID, 1, Length(LCurrentViewDataNode.ParentID))) then
      begin
        LSQLType := '';
        if (Pos('_', ATabsheetID) > 0) then
          LSQLType := Copy(ATabsheetID, Pos('_', ATabsheetID) + 1, Length(ATabsheetID));
        if (Length(LCurrentViewDataNode.ParentID) = Length(ATabsheetID)) or
           (
             (not Assigned(LCurrentViewDataNode.ViewDataSet[0])) or
             (LCurrentViewDataNode.ViewDataSet[0].SQLType = StrToInt(LSQLType))
           ) then
        begin

          // Add the tree node.
          LViewDataTreeNodeData := TViewDataTreeNodeData.Create;
          LViewDataTreeNodeData.ViewDataNode := LCurrentViewDataNode;
          LViewDataTreeNodeData.TreeNode :=
            ATreeNodes.AddChildObject(nil, LCurrentViewDataNode.ViewID, LViewDataTreeNodeData);
          FParentNodesIds.AddObject(LCurrentViewDataNode.ViewID,LViewDataTreeNodeData.TreeNode);
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.PopulateChildrenTreeNodes(ATreeNodes: TTreeNodes; ATabsheetID: string);
const OPNAME = 'TViewDataList.PopulateChildrenTreeNodes';
var
  LViewDataNodesIndex: integer;
  LCurrentViewDataNode: TViewDataNode;
  LViewDataTreeNodeData: TViewDataTreeNodeData;
  LParentTreeNode: TTreeNode;
  LSQLType: string;
begin
  try

    // Add the children nodes.
    for LViewDataNodesIndex := 0 to FViewDataNodes.Count - 1 do
    begin

      // Find the parent node.
      LCurrentViewDataNode := TViewDataNode(FViewDataNodes[LViewDataNodesIndex]);
      if(FAppModules.StudyArea.ModelVersion <> '7') then
      begin
        if((UpperCase(LCurrentViewDataNode.ViewID) = 'IRRIGATIONBLOCK')) then Continue;
        if((UpperCase(LCurrentViewDataNode.ViewID) = 'WETLAND')) then Continue;
        if((UpperCase(LCurrentViewDataNode.ViewID) = 'REVIEWWETLAND')) then Continue;
      end;

      LParentTreeNode := FindParentTreeNode(ATreeNodes, LCurrentViewDataNode.ParentID);
      if Assigned(LParentTreeNode) then
      begin
        if not CanSubNodeBeAddedToTree(LCurrentViewDataNode,LParentTreeNode,ATabsheetID) then
          Continue;

        // Check if the SQL type is allowed.
        LSQLType := '';
        if (Pos('_', ATabsheetID) > 0) then
          LSQLType := Copy(ATabsheetID, Pos('_', ATabsheetID) + 1, Length(ATabsheetID));
        if (LSQLType = '') or
           (
             (not Assigned(LCurrentViewDataNode.ViewDataSet[0])) or
             (LCurrentViewDataNode.ViewDataSet[0].SQLType = StrToInt(LSQLType))
           ) then
        begin

          // Add the tree node.
          LViewDataTreeNodeData := TViewDataTreeNodeData.Create;
          LViewDataTreeNodeData.ViewDataNode := LCurrentViewDataNode;
          LViewDataTreeNodeData.TreeNode :=
            ATreeNodes.AddChildObject(LParentTreeNode, LCurrentViewDataNode.ViewID, LViewDataTreeNodeData);
          FParentNodesIds.AddObject(LCurrentViewDataNode.ViewID,LViewDataTreeNodeData.TreeNode);
        end;
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.FindParentTreeNode(ATreeNodes: TTreeNodes; AViewID: string): TTreeNode;
const OPNAME = 'TViewDataList.FindParentTreeNode';
var LTreeNodeIndex: integer;
begin
  Result := nil;
  try

    LTreeNodeIndex := FParentNodesIds.IndexOf(AViewID);
    if(LTreeNodeIndex >= 0) then
      Result := TTreeNode(FParentNodesIds.Objects[LTreeNodeIndex]);
    {
    for LTreeNodeIndex := 0 to ATreeNodes.Count - 1 do
    begin
      if Assigned(ATreeNodes[LTreeNodeIndex].Data) then
      begin
        if (TViewDataTreeNodeData(ATreeNodes[LTreeNodeIndex].Data).ViewDataNode.ViewID = AViewID) then
        begin
          Result := ATreeNodes[LTreeNodeIndex];
          break;
        end;
      end;
    end;
    }
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.SetImageIndexes(ATreeNodes: TTreeNodes);
const OPNAME = 'TViewDataList.SetImageIndexes';
var
  LIndex: integer;
  LImageList: IStringImageList;
  LBitmapName: string;
  LNode: TTreeNode;
begin
  try
    if ATreeNodes.Owner.GetInterface(IStringImageList, LImageList) then
    begin
      for LIndex := 0 to ATreeNodes.Count - 1 do
      begin
        if Assigned(ATreeNodes.Item[LIndex].Data) and
           Assigned(TViewDataTreeNodeData(ATreeNodes.Item[LIndex].Data).ViewDataNode) then
        begin
          LBitmapName := TViewDataTreeNodeData(ATreeNodes.Item[LIndex].Data).ViewDataNode.BitmapName;
          LNode := TTreeNodes(ATreeNodes).Item[LIndex];
          LNode.StateIndex := LImageList.AddResourceBitmap(LBitmapName);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.GetCurrentTreeNodeData: TObject;
const OPNAME = 'TViewDataList.GetCurrentTreeNodeData';
begin
  Result := nil;
  try
    Result := FCurrentTreeNodeData;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.SetCurrentTreeNodeData(ATreeNodeData: TObject);
const OPNAME = 'TViewDataList.SetCurrentTreeNodeData';
begin
  try
    FCurrentTreeNodeData := TViewDataTreeNodeData(ATreeNodeData);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TViewDataList.PopulateJumpList(AJumpList: TStringList);
const OPNAME = 'TViewDataList.PopulateJumpList';
var
  LViewDataNodesIndex: integer;
  LViewDataJumpToNodesIndex: integer;
  LCurrentViewDataNode: TViewDataNode;
begin
  try

    // Loop for all the nodes.
    AJumpList.Clear;
    for LViewDataNodesIndex := 0 to FViewDataNodes.Count - 1 do
    begin

      // Loop for all the jumps of this node
      LCurrentViewDataNode := TViewDataNode(FViewDataNodes[LViewDataNodesIndex]);
      for LViewDataJumpToNodesIndex := 0 to LCurrentViewDataNode.JumpCount - 1 do
      begin

        // Add the jumps for this node.
        AJumpList.AddObject(
          LCurrentViewDataNode.ViewID,
          LCurrentViewDataNode.Jump[LViewDataJumpToNodesIndex]);
      end;
    end;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.StudyDataHasChanged(AContext: TChangeContext;
  AFieldName, AOldValue, ANewValue: string): boolean;
const OPNAME = 'TViewDataList.StudyDataHasChanged';
var
  LIndex: integer;
  //LCurrentViewDataNode: TViewDataNode;
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName, AOldValue, ANewValue);
  try
    for LIndex := 0 to FParentNodesIds.Count -1 do
    begin
      if(FParentNodesIds.Strings[LIndex] = AOldValue) then
         FParentNodesIds.Strings[LIndex] := ANewValue;
    end;

    {for LIndex := 0 to FViewDataNodes.Count - 1 do
    begin
      LCurrentViewDataNode := TViewDataNode(FViewDataNodes.Items[LIndex]);
      if(LCurrentViewDataNode.ViewID = AOldValue) then
        LCurrentViewDataNode.ViewID := ANewValue;
      if(LCurrentViewDataNode.ParentID = AOldValue) then
        LCurrentViewDataNode.ParentID := ANewValue;
      if(LCurrentViewDataNode.TopParentID = AOldValue) then
        LCurrentViewDataNode.TopParentID := ANewValue;
      if(LCurrentViewDataNode.OverrideCaption = AOldValue) then
        LCurrentViewDataNode.OverrideCaption := ANewValue;
    end;}
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.Get_ViewDataNodeByIndex(AIndex: integer): TViewDataNode;
const OPNAME = 'TViewDataList.Get_ViewDataNodeByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FViewDataNodes.Count) then
      Result := TViewDataNode(FViewDataNodes[AIndex]);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.Get_ViewDataNodeCount: integer;
const OPNAME = 'TViewDataList.Get_ViewDataNodeCount';
begin
  Result := 0;
  try
    Result := FViewDataNodes.Count;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TViewDataList.CanSubNodeBeAddedToTree(AViewDataNode: TViewDataNode; AParentTreeNode: TTreeNode; ATabsheetID: string): boolean;
const OPNAME = 'TViewDataList.CanSubNodeBeAddedToTree';
begin
  Result := True;
  try
    if(ATabsheetID = 'Review')  and (FAppModules.StudyArea.ModelCode = CPlanning) then
    begin

      if(UpperCase(AViewDataNode.ParentID) = 'REVIEWARESERVOIR') then
        Result := TAbstractModelData(FAppModules.Model.ModelData).ViewDataItemExist('ReservoirNames',AViewDataNode.OverrideCaption);

      if(UpperCase(AViewDataNode.ParentID) = 'REVIEWMASTERCONTROLCONFIGURATION') then
      begin
        if(AViewDataNode.OverrideCaption = '') then
          Result := False
        else
          Result := TAbstractModelData(FAppModules.Model.ModelData).ViewDataItemExist('ChannelNames',AViewDataNode.OverrideCaption);
      end;

      if(Pos('REVIEWCHANNELDETAILS',UpperCase(AViewDataNode.ParentID)) = 1) then
      begin
        if(AViewDataNode.OverrideCaption = '') then
          Result := False
        else
          Result := TAbstractModelData(FAppModules.Model.ModelData).ViewDataItemExist('ChannelNames',AViewDataNode.OverrideCaption);
      end;

      if(Pos('REVIEWWRPMCHANNELDETAILS',UpperCase(AViewDataNode.ParentID)) = 1) then
      begin
        if(AViewDataNode.OverrideCaption = '') then
          Result := False
        else
          Result := TAbstractModelData(FAppModules.Model.ModelData).ViewDataItemExist('ChannelNames',AViewDataNode.OverrideCaption);
      end;

      if(UpperCase(AViewDataNode.ParentID) = 'REVIEWSUBSYSTEMSTORAGE') then
        Result := TAbstractModelData(FAppModules.Model.ModelData).ViewDataItemExist('SubSystemNames',AViewDataNode.OverrideCaption);
      if(UpperCase(AViewDataNode.ParentID) = 'REVIEWSUBSYSTEMCURTAILMENT') then
        Result := TAbstractModelData(FAppModules.Model.ModelData).ViewDataItemExist('SubSystemNames',AViewDataNode.OverrideCaption);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
