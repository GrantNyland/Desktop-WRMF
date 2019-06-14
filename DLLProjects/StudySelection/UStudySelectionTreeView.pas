//
//
//  UNIT      : Contains the class TStudySelectionTreeView.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/16
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStudySelectionTreeView;

interface

uses
  Vcl.ComCtrls,
  UStudyList,
  UAbstractComponent;

type
  TStudySelectionTreeView = class(TAbstractTreeView)
  protected
    FStudyList: TStudyList;
  public
    procedure SetStudyList(AStudyList: TStudyList);
    function FindFirstSubAreaNodeAfter(AModelName, ASubAreaName: string; ADateLaterThan: TDateTime): TTreeNode;
    function FindFirstNodeByLabel(AStudyLabel, AModelLabel, ASubAreaLabel, AScenarioLabel: string): TTreeNode;
    function PopulateStudyArea(ANode: TTreeNode): boolean;
    function MoreThanOneNodeAtLevel(ANode: TTreeNode): boolean;
  end;

implementation

uses
  SysUtils,
  UStudyObjects,
  UErrorHandlingOperations;

procedure TStudySelectionTreeView.SetStudyList(AStudyList: TStudyList);
const OPNAME = 'TStudySelectionTreeView.SetStudyList';
var
  LStudyIndex, LModelIndex, LSubAreaIndex, LScenarioIndex: integer;
  LStudyNode, LModelNode, LSubAreaNode, LScenarioNode: TTreeNode;
begin
  try

    // Clear all the existing data.
    Items.Clear;
    FStudyList := AStudyList;

    // Add the tree nodes.
    for LStudyIndex := 0 to FStudyList.StudyCount - 1 do
    begin
      LStudyNode := Items.AddObject(nil,
        FStudyList[LStudyIndex].StudyLabel,
        FStudyList[LStudyIndex]);
      FStudyList[LStudyIndex].Node := LStudyNode;
      for LModelIndex := 0 to FStudyList[LStudyIndex].ModelCount - 1 do
      begin
        LModelNode := Items.AddChildObject(LStudyNode,
          FStudyList[LStudyIndex].Model[LModelIndex].ModelLabel,
          FStudyList[LStudyIndex].Model[LModelIndex]);
        FStudyList[LStudyIndex].Model[LModelIndex].Node := LModelNode;
        for LSubAreaIndex := 0 to FStudyList[LStudyIndex].Model[LModelIndex].SubAreaCount - 1 do
        begin
          LSubAreaNode := Items.AddChildObject(LModelNode,
            FStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].SubAreaLabel,
            FStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex]);
          FStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].Node := LSubAreaNode;
          for LScenarioIndex := 0 to FStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].ScenarioCount - 1 do
          begin
            LScenarioNode := Items.AddChildObject(LSubAreaNode,
              FStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].Scenario[LScenarioIndex].ScenarioLabel,
              FStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].Scenario[LScenarioIndex]);
            FStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].Scenario[LScenarioIndex].Node := LScenarioNode;
          end;
        end;
      end;
    end;

    // Expand all the nodes.
    //for LStudyIndex := 0 to Items.Count - 1 do
    //  Items[LStudyIndex].Expand(True);

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudySelectionTreeView.FindFirstSubAreaNodeAfter(AModelName, ASubAreaName: string; ADateLaterThan: TDateTime): TTreeNode;
const OPNAME = 'TStudySelectionTreeView.FindFirstSubAreaNodeAfter';
var LSubArea: TSubAreaDataObject;
begin
  Result := nil;
  try
    LSubArea := FStudyList.FindFirstSubAreaAfter(AModelName, ASubAreaName, ADateLaterThan);
    if Assigned(LSubArea) then
      Result := LSubArea.Node;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudySelectionTreeView.FindFirstNodeByLabel(AStudyLabel, AModelLabel, ASubAreaLabel, AScenarioLabel: string): TTreeNode;
const OPNAME = 'TStudySelectionTreeView.FindFirstNodeByLabel';
begin
  Result := nil;
  try
    Result := FStudyList.FindFirstObjectByLabel(AStudyLabel, AModelLabel, ASubAreaLabel, AScenarioLabel).Node;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudySelectionTreeView.PopulateStudyArea(ANode: TTreeNode): boolean;
const OPNAME = 'TStudySelectionTreeView.PopulateStudyArea';
var LScenarioData: TScenarioDataObject;
begin
  Result := False;
  try
    if Assigned(ANode) and Assigned(ANode.Data) then
    begin
      case TScenarioDataObject(ANode.Data).NodeLevel of
        nlStudy    : LScenarioData := TStudyDataObject(ANode.Data).Model[0].SubArea[0].Scenario[0];
        nlModel    : LScenarioData := TModelDataObject(ANode.Data).SubArea[0].Scenario[0];
        nlSubArea  : LScenarioData := TSubAreaDataObject(ANode.Data).Scenario[0];
        nlScenario : LScenarioData := TScenarioDataObject(ANode.Data);
      else
        raise Exception.CreateFmt('Unknown node level [%d].', [integer(TScenarioDataObject(ANode.Data).NodeLevel)]);
      end;
      FStudyList.StudyArea.CopyFromScenarioDataObject(LScenarioData);
      Result := True;
    end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudySelectionTreeView.MoreThanOneNodeAtLevel(ANode: TTreeNode): boolean;
const OPNAME = 'TStudySelectionTreeView.MoreThanOneNodeAtLevel';
var
  LParentNode: TTreeNode;
begin
  Result := False;
  try
    if Assigned(ANode) then
    begin
      LParentNode := ANode.Parent;
      if Assigned(LParentNode) then
        Result := (LParentNode.Count > 1)
      else
        Result := (FStudyList.StudyCount > 1);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
