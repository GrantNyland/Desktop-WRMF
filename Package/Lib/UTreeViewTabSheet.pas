//
//
//  UNIT      : Contains TTreeViewTabSheet Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/01
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UTreeViewTabSheet;

interface

uses
  Classes,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  UMenuItemManager,
  UAbstractComponent;

const
  C_DefaultTreeViewWidth = 100;
  C_MinimumTreeViewWidth = 40;
  C_IdSplitterPos = 'SplitterPos';

type
  TTreeViewTabSheet = class(TAbstractTabSheet)
  protected
    FTreeView: TTreeView;
    FSplitter: TSplitter;
    FTreeViewNodes: TStringList;
    FMenuItemManager: TMenuItemManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure SelectNodeByText(AText: string);
    function  VisibleCount: integer;
    procedure DeleteEmptyParentNodes; virtual;
    procedure OnTreeNodeHasBeenAdded(Sender: TObject; Node: TTreeNode);
    procedure OnTreeNodeHasBeenDeleted(Sender: TObject; Node: TTreeNode);
    function GetNodeByIndex(AIndex: integer): TTreeNode;
    function GetMainNodeByName(AName: string):  TTreeNode;
    function FindParentNode(AParentIDsCommaText : string;AParentWeighting  : integer) : TTreeNode;
  public
    function SaveState: boolean; override;
    function ResetState: boolean; override;
    procedure PopulateTreeView; virtual;
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
    procedure SetMenuVisible(AVisible: boolean); override;
    function AddAndSelectElement(AViewID     : string; AParentIDsCommaText : string;
                                 AWeighting, AParentWeighting  : integer;
                                 ACaption    : string; ABitmapName         : string; ADataType   : string;
                                 ASelect     : boolean): boolean;
    function DeleteTreeNode(AViewID : string; AParentIDsCommaText : string;AWeighting,AParentWeighting: integer): boolean;
    function ReselectTreeNode (ATreeNode : TTreeNode) : boolean;
    function LanguageHasChanged: boolean; override;
    function NodeCount: integer;
    property NodeByIndex[AIndex: integer]: TTreeNode read GetNodeByIndex;
    property TreeView: TTreeView read FTreeView;
    property MainNodeByName[AName: string]: TTreeNode read GetMainNodeByName;
  end;

implementation

uses
  SysUtils,
  Vcl.Controls,
  Contnrs,
  UConstants,
  UViewDataItem,
  UErrorHandlingOperations;

procedure TTreeViewTabSheet.CreateMemberObjects;
const OPNAME = 'TTreeViewTabSheet.CreateMemberObjects';
begin
  try
    FTreeViewNodes := TStringList.Create;
    FTreeViewNodes.Sorted := True;
    FTreeViewNodes.Duplicates := dupAccept;

    // Create the tree view.
    FTreeView := TAbstractTreeView.Create(self, FAppModules);
    FTreeView.Parent := self;
    FTreeView.Align := alLeft;
    FTreeView.Width := C_DefaultTreeViewWidth;
    FTreeView.Constraints.MinWidth := C_MinimumTreeViewWidth;
    FTreeView.HideSelection := False;
    FTreeView.OnAddition := OnTreeNodeHasBeenAdded;
    FTreeView.OnDeletion := OnTreeNodeHasBeenDeleted;

    // Create the splitter.
    FSplitter := TSplitter.Create(self);
    FSplitter.Parent := self;
    FSplitter.Width := 4;
    FSplitter.Left := FTreeView.Left + FTreeView.Width + 1;
    FSplitter.Align := AlLeft;
    FSplitter.MinSize := C_MinimumTreeViewWidth;

    // Set the menu item manager to nil incase a descendant does not create one.
    FMenuItemManager := nil;

  // Handle exceptions.
  except on E : Exception do HandleError(E, OPNAME); end;
end;

procedure TTreeViewTabSheet.DestroyMemberObjects;
const OPNAME = 'TTreeViewTabSheet.DestroyMemberObjects';
begin
  try
    FTreeView.OnAddition := nil;
    FTreeView.OnDeletion := nil;
    FreeAndNil(FTreeViewNodes);
    FreeAndNil(FMenuItemManager);
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTreeViewTabSheet.AfterConstruction;
const OPNAME = 'TTreeViewTabSheet.AfterConstruction';
begin
  try
    inherited;
    try
      FTreeView.Width := FAppModules.ViewIni.ReadInteger(ClassName, C_IdSplitterPos, C_DefaultTreeViewWidth);
    except end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTreeViewTabSheet.BeforeDestruction;
const OPNAME = 'TTreeViewTabSheet.BeforeDestruction';
begin
  try
    try
      SaveState;
    except end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TTreeViewTabSheet.SetMenuVisible(AVisible: boolean);
const OPNAME = 'TTreeViewTabSheet.SetMenuVisible';
begin
  try
    if Assigned(FMenuItemManager) then
    begin
      if AVisible then
      begin
        FMenuItemManager.Show;
      end
      else
      begin
        FMenuItemManager.Hide;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTreeViewTabSheet.LanguageHasChanged: boolean;
const OPNAME = 'TTreeViewTabSheet.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Result and Assigned(FMenuItemManager) then
      Result := FMenuItemManager.LanguageHasChanged;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTreeViewTabSheet.NodeCount: integer;
const OPNAME = 'TTreeViewTabSheet.NodeCount';
begin
  Result := 0;
  try
    Result := FTreeViewNodes.Count;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTreeViewTabSheet.SelectNodeByText(AText: string);
const OPNAME = 'TTreeViewTabSheet.SelectNodeByText';
var LIndex: integer;
begin
  try
    LIndex := FTreeViewNodes.IndexOf(AText);
    if(LIndex >= 0) and Assigned(FTreeViewNodes.Objects[LIndex]) then
      FTreeView.Selected := TTreeNode(FTreeViewNodes.Objects[LIndex]);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTreeViewTabSheet.VisibleCount: integer;
const OPNAME = 'TTreeViewTabSheet.VisibleCount';
var LIndex: integer;
begin
  Result := 0;
  try
    for LIndex := 0 to Self.ControlCount - 1 do
    begin
      if Self.Controls[LIndex].Visible then
        Result := Result + 1;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTreeViewTabSheet.PopulateTreeView;
const OPNAME = 'TTreeViewTabSheet.PopulateTreeView';
begin
  try
    FTreeViewNodes.Clear;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTreeViewTabSheet.OnTreeNodeHasBeenAdded(Sender: TObject; Node: TTreeNode);
const OPNAME = 'TTreeViewTabSheet.OnTreeNodeHasBeenAdded';
begin
  try
    FTreeViewNodes.AddObject(Node.Text,Node);
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTreeViewTabSheet.OnTreeNodeHasBeenDeleted(Sender: TObject; Node: TTreeNode);
const OPNAME = 'TTreeViewTabSheet.OnTreeNodeHasBeenDeleted';
var
  LIndex: integer;
  LNode: TTreeNode;
begin
  try
    LIndex := FTreeViewNodes.IndexOf(Node.Text);
    if(LIndex >= 0) then
    begin
      for LIndex := LIndex to FTreeViewNodes.Count -1 do
      begin
        LNode := NodeByIndex[LIndex];
        if (LNode.Text <> Node.Text)  then
          Break;
        //if(LNode.AbsoluteIndex = Node.AbsoluteIndex) then
        if(LNode = Node) then
        begin
          FTreeViewNodes.Delete(LIndex);
          Break;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTreeViewTabSheet.ResetState: boolean;
const OPNAME = 'TTreeViewTabSheet.ResetState';
begin
  Result := inherited ResetState;
  try
    FTreeView.Width := C_DefaultTreeViewWidth;
    FTreeView.Constraints.MinWidth := C_MinimumTreeViewWidth;
    FSplitter.Left := FTreeView.Left + FTreeView.Width + 1;
    FSplitter.MinSize := C_MinimumTreeViewWidth;
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTreeViewTabSheet.SaveState: boolean;
const OPNAME = 'TTreeViewTabSheet.SaveState';
begin
  Result := inherited SaveState;
  try
    FAppModules.ViewIni.WriteInteger(ClassName, C_IdSplitterPos, FTreeView.Width);
    Result := True;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTreeViewTabSheet.GetNodeByIndex(AIndex: integer): TTreeNode;
const OPNAME = 'TTreeViewTabSheet.GetNodeByIndex';
begin
  Result := nil;
  try
    if(AIndex >= 0) and (AIndex < FTreeViewNodes.Count) then
    begin
      if Assigned(FTreeViewNodes.Objects[AIndex]) then
        Result := TTreeNode(FTreeViewNodes.Objects[AIndex]);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TTreeViewTabSheet.DeleteEmptyParentNodes;
const OPNAME = 'TTreeViewTabSheet.DeleteEmptyParentNodes';
var
  LCountainer: TObjectList;
  LIndex: integer;
  LNode: TTreeNode;
begin
  try
    LCountainer := TObjectList.Create(False);
    FTreeView.Items.BeginUpdate;
    try
      LCountainer.Clear;
      for LIndex := 0 to FTreeViewNodes.Count -1 do
      begin
        LNode := TTreeNode(FTreeViewNodes.Objects[LIndex]);
        if Assigned(LNode) then
        begin
          if (LNode.Level = 1) and (LNode.Count = 0) then
             LCountainer.Add(LNode);
        end;
      end;
      for LIndex := 0 to  LCountainer.Count -1 do
      begin
        LNode := TTreeNode(LCountainer.Items[LIndex]);
        FTreeView.Items.Delete(LNode);
      end;
    finally
      FTreeView.Items.EndUpdate;
      LCountainer.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTreeViewTabSheet.GetMainNodeByName(AName: string): TTreeNode;
const OPNAME = 'TTreeViewTabSheet.GetMainNodeByName';
var
  LIndex    : integer;
  LNode     : TTreeNode;
  LNodeData : TViewDataTreeNodeData;
  LViewData : TViewDataNode;
begin
  Result := nil;
  try
    for LIndex := 0 to FTreeViewNodes.Count-1 do
    begin
      if Assigned(FTreeViewNodes.Objects[LIndex]) then
      begin
        LNode := TTreeNode(FTreeViewNodes.Objects[LIndex]);
        if(LNode.Level = 0) then
        begin
          if Assigned(LNode.Data) then
          begin
            LNodeData := TViewDataTreeNodeData(LNode.Data);
            LViewData := LNodeData.ViewDataNode;
            if(UpperCase(LViewData.ViewID) = UpperCase(AName)) then
              Result := LNode;
          end;
        end;
      end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TTreeViewTabSheet.FindParentNode(AParentIDsCommaText : string;AParentWeighting  : integer) : TTreeNode;
const OPNAME = 'TTreeViewTabSheet.FindParentNode';
var
  LCount,
  LIndex          : integer;
  LNode           : TTreeNode;
  LParentNode     : TTreeNode;
  LNodeData       : TViewDataTreeNodeData;
  LParentNodeData : TViewDataTreeNodeData;
  LViewData       : TViewDataNode;
  LParentViewData : TViewDataNode;
  LIDContainer    : TStringList;
  LParentExist    : boolean;
begin
  Result := nil;
  try
    LIDContainer := TStringList.Create;
    try
      LIDContainer.CommaText := AParentIDsCommaText;
      if(LIDContainer.Count > 0) then
      begin
        for LIndex := 0 to FTreeViewNodes.Count-1 do
        begin
          LNode := TTreeNode(FTreeViewNodes.Objects[LIndex]);
          if Assigned(LNode.Data) then
          begin
            LNodeData := TViewDataTreeNodeData(LNode.Data);
            LViewData := LNodeData.ViewDataNode;
            if(UpperCase(LViewData.ViewID) = UpperCase(LIDContainer[LIDContainer.Count-1])) then
            begin
              if(AParentWeighting  <> NullInteger) then
              begin
                if(LViewData.Weighting <> AParentWeighting) then
                  Continue;
              end;

              LParentExist := True;
              LParentNode  := LNode;
              for LCount := LIDContainer.Count-2 downto 0 do
              begin
                LParentNode := LParentNode.Parent;
                if(LParentNode = nil) then
                begin
                  LParentExist := False;
                  Break;
                end;
                if Assigned(LParentNode.Data) then
                begin
                  LParentNodeData := TViewDataTreeNodeData(LParentNode.Data);
                  LParentViewData := LParentNodeData.ViewDataNode;
                  if(UpperCase(LParentViewData.ViewID) <> UpperCase(LIDContainer[LCount])) then
                  begin
                    LParentExist := False;
                    Break;
                  end;
                end
                else
                begin
                  LParentExist := False;
                  Break;
                end;
              end;

              if LParentExist then
              begin
                Result := LNode;
                Break;
              end;
            end;
          end;
        end;
      end;
    finally
      LIDContainer.Free;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;


function TTreeViewTabSheet.AddAndSelectElement(AViewID,AParentIDsCommaText: string;
          AWeighting,AParentWeighting: integer; ACaption, ABitmapName, ADataType: string; ASelect: boolean): boolean;
const OPNAME = 'TTreeViewTabSheet.AddAndSelectElement';
var
  LViewDataNode : TViewDataNode;
  LNewTreeNode  : TTreeNode;
  LParentNode   : TTreeNode;
  LTreeNodeData : TViewDataTreeNodeData;
  LImageList    : IStringImageList;
begin
  Result := False;
  try
    LParentNode := FindParentNode(AParentIDsCommaText,AParentWeighting);
    if(LParentNode = nil) then
      Exit;

    LViewDataNode := TViewDataNode.Create;
    with LViewDataNode do
    begin
      ViewID          := AViewID;
      ParentID        := TViewDataTreeNodeData(LParentNode.Data).ViewDataNode.ParentID;
      Weighting       := AWeighting;
      OverrideCaption := ACaption;
      BitmapName      := ABitmapName;
      DataType        := ADataType;
    end;

    LTreeNodeData              := TViewDataTreeNodeData.Create;
    LTreeNodeData.ViewDataNode := LViewDataNode;
    LNewTreeNode               := FTreeView.Items.AddChild(LParentNode,ACaption);
    LTreeNodeData.TreeNode     := lNewTreeNode;
    LNewTreeNode.Data          := LTreeNodeData;

    if (Trim(ABitmapName) <> '') then
    begin
      if (FTreeView.GetInterface(IStringImageList, LImageList)) then
        LNewTreeNode.StateIndex := LImageList.AddResourceBitmap(ABitmapName);
    end;

    if(ASelect) then
      FTreeView.Selected := lNewTreeNode;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTreeViewTabSheet.DeleteTreeNode(AViewID,AParentIDsCommaText: string;AWeighting,AParentWeighting: integer): boolean;
const OPNAME = 'TTreeViewTabSheet.DeleteTreeNode';
var
  LIndex        : integer;
  LViewDataNode : TViewDataNode;
  LTreeNode     : TTreeNode;
  LParentNode   : TTreeNode;
  LTreeNodeData : TViewDataTreeNodeData;
  LNodeSelected : boolean;
begin
  Result := False;
  try
    LParentNode := FindParentNode(AParentIDsCommaText,AParentWeighting);
    if(LParentNode = nil) then
      Exit;
    for LIndex := 0 to LParentNode.Count-1 do
    begin
      LTreeNode := LParentNode.Item[LIndex];
      if Assigned(LTreeNode.Data) then
      begin
        LTreeNodeData := TViewDataTreeNodeData(LTreeNode.Data);
        LViewDataNode := LTreeNodeData.ViewDataNode;
        if (AWeighting = LViewDataNode.Weighting) and (UpperCase(LViewDataNode.ViewID) = UpperCase(AViewID)) then
        begin
          LNodeSelected := (FTreeView.Selected = LTreeNode);
          FTreeView.Items.Delete(LTreeNode);
          if LNodeSelected then
             FTreeView.Selected := LParentNode;
          Result := True;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TTreeViewTabSheet.ReselectTreeNode(ATreeNode: TTreeNode): boolean;
const OPNAME = 'TTreeViewTabSheet.ReselectTreeNode';
var
  LTreeNode: TTreeNode;
begin
  Result := False;
  try
    if(ATreeNode = nil) then
      Exit;

    LTreeNode := FTreeView.Items.GetNode(ATreeNode.ItemId);
    if(LTreeNode <> nil) and (LTreeNode = ATreeNode) then
    begin
      FTreeView.Selected := nil;
      FTreeView.Selected := ATreeNode;
      Result := True;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.

