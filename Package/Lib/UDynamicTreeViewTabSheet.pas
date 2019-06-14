//
//
//  UNIT      : Contains TDynamicTreeViewTabSheet Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/03/01
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UDynamicTreeViewTabSheet;

interface

uses
  Classes,
  vcl.ComCtrls,
  vcl.ExtCtrls,
  UMenuItemManager,
  UDataViewerMesgSheet;
type
  TDynamicTreeViewTabSheet = class(TDataViewerMesgSheet)
  protected
    function FindParentNode(AParentIDsCommaText : string;AParentWeighting  : integer) : TTreeNode;
  public
    function AddAndSelectElement(AViewID     : string; AParentIDsCommaText : string;
                                 AWeighting, AParentWeighting  : integer;
                                 ACaption    : string; ABitmapName         : string; ADataType   : string;
                                 ASelect     : boolean): boolean;
    function DeleteTreeNode(AViewID : string; AParentIDsCommaText : string;AWeighting,AParentWeighting: integer): boolean;
    function ReselectTreeNode (ATreeNode : TTreeNode) : boolean;
  end;

implementation

uses
  SysUtils,
  vcl.Controls,
  Contnrs,
  UConstants,
  UViewDataItem,
  UAbstractComponent,
  UErrorHandlingOperations, UTreeViewTabSheet;

function TDynamicTreeViewTabSheet.FindParentNode(AParentIDsCommaText : string;AParentWeighting  : integer) : TTreeNode;
const OPNAME = 'TDynamicTreeViewTabSheet.FindParentNode';
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
        for LIndex := 0 to FTreeView.Items.Count-1 do
        begin
          LNode := FTreeView.Items[LIndex];
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

function TDynamicTreeViewTabSheet.AddAndSelectElement(AViewID,AParentIDsCommaText: string;
          AWeighting,AParentWeighting: integer; ACaption, ABitmapName, ADataType: string; ASelect: boolean): boolean;
const OPNAME = 'TDynamicTreeViewTabSheet.AddAndSelectElement';
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

function TDynamicTreeViewTabSheet.DeleteTreeNode(AViewID,AParentIDsCommaText: string;AWeighting,AParentWeighting: integer): boolean;
const OPNAME = 'TDynamicTreeViewTabSheet.DeleteTreeNode';
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

function TDynamicTreeViewTabSheet.ReselectTreeNode(ATreeNode: TTreeNode): boolean;
const OPNAME = 'TDynamicTreeViewTabSheet.ReselectTreeNode';
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

