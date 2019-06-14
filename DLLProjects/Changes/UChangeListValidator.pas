{******************************************************************************}
{*  UNIT      : Contains the class TChangeListValidator.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/11                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChangeListValidator;

interface

uses
  Classes,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChangeData,
  VoaimsCom_TLB,
  UChangeListDialog;

type
  TChangeListValidator = class(TAbstractDataDialogValidator)
  private
    FSystemFlag    : boolean;
    FFlagUp        : boolean;
    FElementID     : integer;
    FChangeGroupID : integer;
    FElementType   : TChangeElementType;
    FParentGroupID : integer;
    FGroupNameChanging : boolean;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnTreeViewClick(Sender: TObject);
    procedure DoTreeViewChange (Sender: TObject; Node: TTreeNode);
    procedure DoTreeViewEnter (Sender : TObject);
    procedure DoTrvChangeGroupMouseDown (Sender : TObject;
                                        Button : TMouseButton;
                                        Shift  : TShiftState;
                                        X, Y   : Integer);
    procedure OnTrvChangesDragOver (Sender, Source : TObject;
                                    X, Y           : Integer;
                                    State          : TDragState;
                                    var Accept     : Boolean);
    procedure OnTrvChangesDragDrop (Sender, Source : TObject;
                                    X, Y           : Integer);
    procedure RePopulateDataViewer;
    procedure PopulateTreeView;
    procedure PopulateGroupNode (ANode             : TTreeNode;
                                 AGroup            : IChangeGroup;
                                 var ASelectedNode : TTreeNode);
    procedure PopulateControls;
    procedure PopulateGrid;
    procedure ResetButtons;
    procedure UpdateChangeListName;
    procedure UpdateChangeGroupName;
    procedure UpdateCreatedBy;
    procedure UpdateDescription;
    procedure ResetNodeImage;
    function TreeNodeType(ANode : TTreeNode) : TChangeElementType;
    function GetSelectedNode(ATreeView : TAbstractTreeView) : TTreeNode;
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext : TChangeContext;
                                  AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function ChangeListDialog: TChangeListDialog;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    property ElementID     : integer            read FElementID;
    property ElementType   : TChangeElementType read FElementType;
    property ParentGroupID : integer            read FParentGroupID;
  end;

implementation

uses
  SysUtils,
  Vcl.Dialogs,
  UErrorHandlingOperations;

{******************************************************************************}
{* TChangeListValidator                                                       *}
{******************************************************************************}

procedure TChangeListValidator.CreateMemberObjects;
const OPNAME = 'TChangeListValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSystemFlag    := FALSE;
    FFlagUp        := FALSE;
    FElementID     := 0;
    FChangeGroupID := 1;
    FElementType   := cetNone;
    FParentGroupID := -1;
    FPanel         := TChangeListDialog.Create(nil,FAppModules);
    with ChangeListDialog do
    begin
      EdtName.OnExit         := OnEditControltExit;
      EdtName.OnEnter        := OnEditControlEnter;

      EdtGroupName.OnExit    := OnEditControltExit;
      EdtGroupName.OnEnter   := OnEditControlEnter;

      EdtPerson.OnExit       := OnEditControltExit;
      EdtPerson.OnEnter      := OnEditControlEnter;

      MmoDescription.OnExit  := OnEditControltExit;
      MmoDescription.OnEnter := OnEditControlEnter;

      TrvChangeGroup.OnClick     := OnTreeViewClick;
      TrvChangeGroup.OnChange    := DoTreeViewChange;

      TrvChangeGroup.OnEnter     := DoTreeViewEnter;

      TrvChangeGroup.OnMouseDown := DoTrvChangeGroupMouseDown;
      TrvChangeGroup.OnDragOver  := OnTrvChangesDragOver;
      TrvChangeGroup.OnDragDrop  := OnTrvChangesDragDrop;

      TrvChangeList.OnChange     := DoTreeViewChange;
      TrvChangeList.OnEnter      := DoTreeViewEnter;
      TrvChangeList.OnClick     := OnTreeViewClick;

      GrdParamChanges.OnEnter := OnEditControlEnter;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.DestroyMemberObjects;
const OPNAME = 'TChangeListValidator.DestroyMemberObjects';
begin
  try
    FSystemFlag := TRUE;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.Initialise: boolean;
const OPNAME = 'TChangeListValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    FGroupNameChanging := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.LanguageHasChanged: boolean;
const OPNAME = 'TChangeListValidator.LanguageHasChanged';
begin
  Result := False;
  try
    Result := ChangeListDialog.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.ClearDataViewer;
const OPNAME = 'TChangeListValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with ChangeListDialog do
    begin
      EdtName.SetFieldValue('');
      EdtGroupName.SetFieldValue('');
      EdtPerson.SetFieldValue('');
      LblDateCreatedValue.Caption := '';
      MmoDescription.Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.PopulateDataViewer;
const OPNAME = 'TChangeListValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.RePopulateDataViewer;
const OPNAME = 'TChangeListValidator.RePopulateDataViewer';
begin
  try
    PopulateTreeView;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.PopulateGroupNode (ANode             : TTreeNode;
                                                  AGroup            : IChangeGroup;
                                                  var ASelectedNode : TTreeNode);
const OPNAME = 'TChangeListValidator.PopulateGroupNode';
var
  lCount      : integer;
  lElement    : IChangeGroupElement;
  lChildGroup : IChangeGroup;
  lChildList  : IChangeList;
  lChildNode  : TTreeNode;
begin
  try
    for lCount := 0 to AGroup.ElementCount - 1 do
    begin
      lElement := AGroup.ChangeGroupElementByIndex(lCount);
      if (lElement.IsElementGroup) then
      begin
        lChildGroup := FAppModules.Changes.ChangeGroupWithID(lElement.ElementID);
        if (lChildGroup <> nil) then
        begin
          lChildNode  := ChangeListDialog.TrvChangeGroup.Items.AddChildObject(ANode, lChildGroup.GroupName, TObject(lElement.ElementID));
          if (lElement.ElementActive) then
          begin
            lChildNode.ImageIndex    := 4;
            lChildNode.SelectedIndex := 4;
          end
          else
          begin
            lChildNode.ImageIndex    := 3;
            lChildNode.SelectedIndex := 3;
          end;
          if ((ASelectedNode = nil) AND (FElementID <> 0) AND
              (FElementType in [cetInactiveGroup, cetActiveGroup]) AND
              (FElementID = lChildGroup.GroupID)) then
            ASelectedNode := lChildNode;
          PopulateGroupNode(lChildNode, lChildGroup, ASelectedNode);
        end;
      end
      else
      begin
        lChildList := FAppModules.Changes.ChangeListWithID(lElement.ElementID);
        if (lChildList <> nil) then
        begin
          lChildNode := ChangeListDialog.TrvChangeGroup.Items.AddChildObject(ANode, lChildList.ChangeListName, TObject(lElement.ElementID));
          if (lElement.ElementActive) then
          begin
            lChildNode.ImageIndex    := 2;
            lChildNode.SelectedIndex := 2;
          end
          else
          begin
            lChildNode.ImageIndex    := 1;
            lChildNode.SelectedIndex := 1;
          end;
          if ((ASelectedNode = nil) AND (FElementID <> 0) AND
              (FElementType in [cetInactiveList, cetActiveList]) AND
              (FElementID = lChildList.ChangeListID)) then
            ASelectedNode := lChildNode;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.PopulateTreeView;
const OPNAME = 'TChangeListValidator.PopulateTreeView';
var
  lLists         : TStringList{TList};
  lChangeList    : IChangeList;
  lMasterGroup   : IChangeGroup;
  lChangeGroup   : IChangeGroup;
  lElement       : IChangeGroupElement;
  lIndex         : integer;
  lNode          : TTreeNode;
  lSelectedListNode  : TTreeNode;
  lSelectedGroupNode  : TTreeNode;
begin
  inherited;
  try
    with ChangeListDialog do
    begin
      lSelectedListNode := GetSelectedNode(TrvChangeList);
      lSelectedGroupNode := GetSelectedNode(TrvChangeGroup);
      FSystemFlag := TRUE;
      TrvChangeGroup.Items.Clear;
      TrvChangeList.Items.Clear;

      lLists := FAppModules.Changes.ChangeLists;
      for lIndex := 0 to lLists.Count - 1 do
      begin
        lChangeList := TChangeList(lLists.Objects[lIndex]);
        lNode := TrvChangeList.Items.AddChildObject(nil, lChangeList.ChangeListName, TObject(lChangeList.ChangeListID));
        if (lChangeList.IsResident) then
        begin
          lNode.ImageIndex    := 0;
          lNode.SelectedIndex := 0;
        end
        else
        begin
          lNode.ImageIndex    := 5;
          lNode.SelectedIndex := 5;
        end;
        if ((lSelectedListNode = nil) AND (FElementID <> 0) AND
            (FElementType = cetChangeList) AND (FElementID = lChangeList.ChangeListID)) then
          lSelectedListNode := lNode;
      end;

      lMasterGroup := FAppModules.Changes.MasterGroup;
      for lIndex := 0 to lMasterGroup.ElementCount - 1 do
      begin
        lElement := lMasterGroup.ChangeGroupElementByIndex(lIndex);
        if (lElement <> nil) AND (lElement.ElementID <> 0) then
        begin
          lChangeGroup := FAppModules.Changes.ChangeGroupWithID(lElement.ElementID);
          if (lChangeGroup <> nil) then
          begin
            lNode := TrvChangeGroup.Items.AddChildObject(nil, lChangeGroup.GroupName, TObject(lChangeGroup.GroupID));
            if (lElement.ElementActive) then
            begin
              lNode.ImageIndex    := 4;
              lNode.SelectedIndex := 4;
            end
            else
            begin
              lNode.ImageIndex    := 3;
              lNode.SelectedIndex := 3;
            end;
            if ((lSelectedGroupNode = nil) AND (FElementID <> 0) AND
                (FElementType in [cetInactiveGroup, cetActiveGroup]) AND
                (FElementID = lChangeGroup.GroupID)) then
              lSelectedGroupNode := lNode;
            PopulateGroupNode(lNode, lChangeGroup, lSelectedGroupNode);
          end;
        end;
      end;
//      FSystemFlag := FALSE;
      if (TrvChangeList.Items.Count > 0) then
      begin
        if Assigned(lSelectedListNode) then
          TrvChangeList.Selected := lSelectedListNode
        else
          TrvChangeList.Selected := TrvChangeList.Items[0];
        DoTreeViewChange(TrvChangeList,TrvChangeList.Selected);
      end;

      if (TrvChangeGroup.Items.Count > 0) then
      begin
        if Assigned(lSelectedGroupNode) then
          TrvChangeGroup.Selected := lSelectedGroupNode
        else
          TrvChangeGroup.Selected := TrvChangeGroup.Items[0];
        DoTreeViewChange(TrvChangeGroup,TrvChangeGroup.Selected);
      end;
      FSystemFlag := FALSE;
      PopulateControls;
      TrvChangeGroup.FullExpand;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.SaveState: boolean;
const OPNAME = 'TChangeListValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.ChangeListDialog : TChangeListDialog;
const OPNAME = 'TChangeListValidator.ChangeListDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TChangeListDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.StudyDataHasChanged (AContext : TChangeContext;
                                                   AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TChangeListValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (NOT FSystemFlag) then
    begin
      if (AFieldName = 'ChangeListName') OR
         (AFieldName = 'ElementActive') OR
         (AFieldName = 'ElementOrder') OR
         (AFieldName = 'ChangeGroupName') then
        PopulateDataViewer
      else
      if (AFieldName = 'ChangeListCopy') then
      begin
        FElementID     := StrToInt(ANewValue);
        FParentGroupID := -1;
        PopulateDataViewer;
      end
      else
      if (AFieldName = 'ChangeGroup') then
      begin
        if (AContext = sdccAdd) then
        begin
          if ChangeListDialog.EdtGroupName.HasValueChanged then
            OnEditControltExit(ChangeListDialog.EdtGroupName);
          FElementID     := StrToInt(ANewValue);
          FParentGroupID := 0;
        end;
        if (AContext = sdccDelete) then
        begin
          FElementID     := 0;
          FElementType   := cetNone;
          FParentGroupID := -1;
        end;
        PopulateDataViewer;
      end
      else
      if (AFieldName = 'ChangeList') then
      begin
        if (AContext = sdccAdd) then
        begin
          FElementID     := StrToInt(ANewValue);
          FElementType   := cetChangeList;
          FParentGroupID := -1;
        end;
        if (AContext = sdccDelete) then
        begin
          FElementID     := 0;
          FElementType   := cetNone;
          FParentGroupID := -1;
        end;
        PopulateDataViewer;
      end
      else
      if (AFieldName = 'ChangeListApply') then
        PopulateDataViewer;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.ResetNodeImage;
const OPNAME = 'TChangeListValidator.ResetNodeImage';
var
  lNode       : TTreeNode;
begin
  try
    with ChangeListDialog do
    begin
      lNode       := TrvChangeGroup.Selected;
      if (lNode.ImageIndex = 4) then
      begin
        lNode.ImageIndex    := 3;
        lNode.SelectedIndex := 3;
      end
      else
      if (lNode.ImageIndex = 3) then
      begin
        lNode.ImageIndex    := 4;
        lNode.SelectedIndex := 4;
      end
      else
      if (lNode.ImageIndex = 2) then
      begin
        lNode.ImageIndex    := 1;
        lNode.SelectedIndex := 1;
      end
      else
      if (lNode.ImageIndex = 1) then
      begin
        lNode.ImageIndex    := 2;
        lNode.SelectedIndex := 2;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.StudyHasChanged: boolean;
const OPNAME = 'TChangeListValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FElementID     := 0;
    FElementType   := cetNone;
    FParentGroupID := -1;
    with ChangeListDialog do
    begin
      EdtName.FieldProperty        := FAppModules.FieldProperties.FieldProperty('ChangeListName');
      EdtGroupName.FieldProperty   := FAppModules.FieldProperties.FieldProperty('ChangeGroupName');
      EdtPerson.FieldProperty      := FAppModules.FieldProperties.FieldProperty('ChangeListCreatedBy');
      MmoDescription.FieldProperty := FAppModules.FieldProperties.FieldProperty('ChangeListDescription');
      ShowControls(FElementType);
    end;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.DoTreeViewEnter (Sender : TObject);
const OPNAME = 'TChangeListValidator.DoTreeViewEnter';
begin
  try
//    if (NOT FFlagUp) then
    DoTreeViewChange(Sender, TAbstractTreeView(Sender).Selected);
//    FFlagUp := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.DoTreeViewChange (Sender : TObject;
                                                 Node   : TTreeNode);
const OPNAME = 'TChangeListValidator.DoTreeViewChange';
var
  lNewElementType : TChangeElementType;
begin
  try
    if (NOT FSystemFlag) AND (Node <> nil) then
    begin
      lNewElementType := TreeNodeType(Node);
      FFlagUp         := (lNewElementType in [0,5]) XOR (FElementType in [0,5]);
      FElementID      := Integer(Node.Data);
      FElementType    := lNewElementType;
      if (Node.Level = 0) then
      begin
        if (FElementType in [cetChangeList,cetAlienList]) then
          FParentGroupID := -1
        else
          FParentGroupID := 0;
      end
      else
        FParentGroupID := Integer(Node.Parent.Data);
    end;
    ChangeListDialog.ShowControls(FElementType);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.DoTrvChangeGroupMouseDown (Sender : TObject;
                                                          Button : TMouseButton;
                                                          Shift  : TShiftState;
                                                          X, Y   : Integer);
const OPNAME = 'TChangeListValidator.DoTrvChangeGroupMouseDown';
var
  lNode       : TTreeNode;
  lHit        : THitTests;
begin
  try
    if (Button = mbLeft) then
    begin
      with ChangeListDialog do
      begin
        lNode := TrvChangeGroup.GetNodeAt(X,Y);
        if (lNode <> nil) then
        begin
          FChangeGroupID := Integer(lNode.Data);
          lHit  := TrvChangeGroup.GetHitTestInfoAt(X, Y);
          if htOnIcon in lHit then
          begin
            if (FElementType = cetInactiveList){(lNode.ImageIndex = 1)} AND
               (lNode.Parent <> nil) AND (TreeNodeType(lNode.Parent) = cetActiveGroup){(lNode.Parent.ImageIndex = 4)} then
              FAppModules.Changes.DoActivateChangeElement(FParentGroupID, FElementID, FALSE)
            else
            if (FElementType = cetActiveList){(lNode.ImageIndex = 2)} then
              FAppModules.Changes.DoDeactivateChangeElement(FParentGroupID, FElementID, FALSE)
            else
            if (FElementType = cetInactiveGroup){(lNode.ImageIndex = 3)} AND
               ((lNode.Parent = nil) OR (TreeNodeType(lNode.Parent) = cetActiveGroup){(lNode.Parent.ImageIndex = 4)}) then
              FAppModules.Changes.DoActivateChangeElement(FParentGroupID, FElementID, TRUE)
            else
            if (FElementType = cetActiveGroup){(lNode.ImageIndex = 4)} then
              FAppModules.Changes.DoDeactivateChangeElement(FParentGroupID, FElementID, TRUE);
          end;
          ResetButtons;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeListValidator.PopulateControls;
const OPNAME = 'TChangeListValidator.PopulateControls';
var
  lChangeList  : IChangeList;
  lChangeGroup : IChangeGroup;
begin
  try
    if (FElementID > 0) then
    begin
      ChangeListDialog.ShowControls(FElementType);
      with ChangeListDialog do
      begin
        case FElementType of
          cetChangeList :
            begin
              lChangeList := FAppModules.Changes.ChangeListWithID(FElementID);
              MmoDescription.Lines.Clear;
              if (lChangeList <> nil) then
              begin
                EdtName.SetFieldValue(lChangeList.ChangeListName);
                LblDateCreatedValue.Caption := DateToStr(lChangeList.DateCreated);
                EdtPerson.SetFieldValue(lChangeList.CreatedBy);
                MmoDescription.SetFieldValue(lChangeList.Description);
                PopulateGrid;
                lChangeList := nil;
              end
              else
              begin
                EdtName.SetFieldValue('');
                LblDateCreatedValue.Caption := '';
                EdtPerson.SetFieldValue('');
              end;
            end;
          cetInactiveGroup, cetActiveGroup :
            begin
              lChangeGroup := FAppModules.Changes.ChangeGroupWithID(FChangeGroupID); //FElementID
              EdtGroupName.Text := '';
              if (lChangeGroup <> nil) then
              begin
                EdtGroupName.SetFieldValue(lChangeGroup.GroupName);
                lChangeGroup := nil;
              end;  
            end;
          else
          begin
          end;
        end;
      end;
    end;
    ResetButtons;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.PopulateGrid;
const OPNAME = 'TChangeListValidator.PopulateGrid';
var
  lIndex         : integer;
  lParamChange   : IParameterChange;
  lChangeList    : IChangeList;
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lFieldIndex    : string;
  lHasMetaData   : Boolean;
begin
  try
    with ChangeListDialog do
    begin
      for lIndex := 1 to GrdParamChanges.RowCount - 1 do
        GrdParamChanges.Rows[lIndex].Clear;
      GrdParamChanges.RowCount := 1;
      if (FAppModules.Model <> nil) AND (FElementID > 0) then
      begin
        lChangeList := FAppModules.Changes.ChangeListWithID(FElementID);
        if (lChangeList <> nil) then
        begin
          GrdParamChanges.RowCount := 1 + lChangeList.ParamChangeCount;
          if (GrdParamChanges.RowCount > 1) then
            GrdParamChanges.FixedRows := 1;

          lFieldProperty := FAppModules.FieldProperties.FieldProperty('ChangeListItem');
          for lIndex := 0 to lChangeList.ParamChangeCount - 1 do
          begin
            lParamChange := lChangeList.ParamChangeByIndex(lIndex);
            lFieldIndex  := IntToStr(lIndex+1);
            lKeyValues   := lChangeList.GetKeyValues(lParamChange.ParamField, lFieldIndex);
            lHasMetaData := FAppModules.MetaData.FindMetaData(lFieldProperty.FieldName, lKeyValues, lFieldIndex) <> nil;
            GrdParamChanges.HasMetaData[1, lIndex+1] := lHasMetaData;

            GrdParamChanges.Cells[0, lIndex+1] := IntToStr(lIndex+1);
            if (lParamChange.ParamField = 'ReservoirPenalty') then
              GrdParamChanges.Cells[1, lIndex+1]
                := lParamChange.ParamField + '[' +
                   FAppModules.Changes.GetKeyValue('Identifier', lParamChange.KeyValues) + ']'
            else
            if (lParamChange.FieldIndex <> '') then
              GrdParamChanges.Cells[1, lIndex+1]
                := lParamChange.ParamField + '[' + lParamChange.FieldIndex + ']'
            else
              GrdParamChanges.Cells[1, lIndex+1] := lParamChange.ParamField;
            GrdParamChanges.Cells[2, lIndex+1] := FAppModules.Model.EntityDescription(lParamChange.ParamField, lParamChange.KeyValues, lParamChange.FieldIndex);
            if (lParamChange.Absolut) then
              GrdParamChanges.Cells[3, lIndex+1] := 'Absolute'
            else
              GrdParamChanges.Cells[3, lIndex+1] := '%';
            GrdParamChanges.Cells[4, lIndex+1] := lParamChange.Change;
            GrdParamChanges.Cells[5, lIndex+1] := lParamChange.ParamDescr;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.ResetButtons;
const OPNAME = 'TChangeListValidator.ResetButtons';
var
  lChangeList  : IChangeList;
  lChangeGroup : IChangeGroup;
  lParentGroup : IChangeGroup;
  lNode        : TTreeNode;
  lParentID    : integer;
begin
  try
    FAppModules.Changes.SetCreateNewChangeList(TRUE);
    FAppModules.Changes.SetDeleteChangeList(FALSE);
    FAppModules.Changes.SetCopyChangeList(FALSE);
    FAppModules.Changes.SetCreateNewChangeGroup(TRUE);
    FAppModules.Changes.SetDeleteChangeGroup(FALSE);

    FAppModules.Changes.SetMoveUpChangeElement(FALSE);
    FAppModules.Changes.SetMoveDownChangeElement(FALSE);
    FAppModules.Changes.SetActivateChangeElement(FALSE);
    FAppModules.Changes.SetDeactivateChangeElement(FALSE);
    FAppModules.Changes.SetApplyChangeList(FALSE);

    FAppModules.Changes.SetImportChangeList(True);
    FAppModules.Changes.SetExportChangeList(FAppModules.Changes.ChangeLists.Count > 0);
    FAppModules.Changes.SetStationFilter(FAppModules.Changes.ChangeLists.Count > 0);

    lNode := ChangeListDialog.TrvChangeGroup.Selected;
    if (lNode <> nil) AND (FElementID > 0) then
    begin
      if (FElementType in [cetChangeList]) then
      begin
        lChangeList := FAppModules.Changes.ChangeListWithID(FElementID);
        FAppModules.Changes.SetDeleteChangeList(lChangeList <> nil);
        FAppModules.Changes.SetCopyChangeList(lChangeList <> nil);
        FAppModules.Changes.SetApplyChangeList((FAppModules.StudyArea.ModelCode <> CRainfall) AND
                                               (lChangeList <> nil));
      end
      else
      if (FElementType in [cetInactiveList, cetActiveList]) then
      begin
        lChangeList := FAppModules.Changes.ChangeListWithID(FElementID);
        if (lChangeList <> nil) then
        begin
          if (FElementType = cetInactiveList) then
          begin
            if (lNode.Parent <> nil) AND (TreeNodeType(lNode.Parent) = cetActiveGroup){(lNode.Parent.ImageIndex = 4)} then
              FAppModules.Changes.SetActivateChangeElement(TRUE);
          end
          else
            FAppModules.Changes.SetDeactivateChangeElement(TRUE);
          if (lNode.Parent <> nil) then
          begin
            lParentID := Integer(lNode.Parent.Data);
            lParentGroup := FAppModules.Changes.ChangeGroupWithID(lParentID);
            if (lParentGroup <> nil) then
            begin
              if (lNode.Index > 0) then
                FAppModules.Changes.SetMoveUpChangeElement(TRUE);
              if (lNode.Index < (lParentGroup.ElementCount - 1)) then
                FAppModules.Changes.SetMoveDownChangeElement(TRUE);
            end;
          end;
        end
      end
      else
      if (FElementType in [cetInactiveGroup, cetActiveGroup]) then
      begin
        lChangeGroup := FAppModules.Changes.ChangeGroupWithID(FElementID);
        if (lChangeGroup <> nil) then
        begin
          FAppModules.Changes.SetDeleteChangeGroup(TRUE);
          if (FElementType = cetInactiveGroup) then
          begin
            if (lNode.Parent = nil) OR (TreeNodeType(lNode.Parent) = cetActiveGroup){(lNode.Parent.ImageIndex = 4)} then
              FAppModules.Changes.SetActivateChangeElement(TRUE);
          end
          else
            FAppModules.Changes.SetDeactivateChangeElement(TRUE);
          if (lChangeGroup.ParentGroupID >= 0) then
          begin
            lParentGroup := FAppModules.Changes.ChangeGroupWithID(lChangeGroup.ParentGroupID);
            if (lParentGroup <> nil) then
            begin
              if (lNode.Index > 0) then
                FAppModules.Changes.SetMoveUpChangeElement(TRUE);
              if (lNode.Index < (lParentGroup.ElementCount - 1)) then
                FAppModules.Changes.SetMoveDownChangeElement(TRUE);
            end;
          end;
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TChangeListValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TChangeListValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with ChangeListDialog do
    begin
      if ((Sender = EdtName) AND (EdtName.HasValueChanged)) then
        UpdateChangeListName
      else
      if ((Sender = EdtGroupName) {AND (EdtGroupName.HasValueChanged)}) then
        UpdateChangeGroupName
      else
      if ((Sender = EdtPerson) AND (EdtPerson.HasValueChanged)) then
        UpdateCreatedBy
      else
      if (Sender = MmoDescription) then
        UpdateDescription;
    end;
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.UpdateChangeListName;
const OPNAME = 'TChangeListValidator.UpdateChangeListName';
var
  lMessage    : string;
  lChangeList : IChangeList;
begin
  try
    if (FElementID > 0) then
    begin
      lChangeList := FAppModules.Changes.ChangeListWithID(FElementID);
      if (lChangeList <> nil) then
      begin
        with ChangeListDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              EdtName.FieldProperty.FieldName, Trim(EdtName.Text), lMessage)) then
          begin
            EdtName.FieldValidationError := lMessage;
            lChangeList.ChangeListName := Trim(EdtName.Text);
            EdtName.SetFieldValue(lChangeList.ChangeListName);
  //          DoContextValidation(dvtChanPropName);
          end
          else
            EdtName.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.UpdateChangeGroupName;
const OPNAME = 'TChangeListValidator.UpdateChangeGroupName';
var
  lMessage     : string;
  lChangeGroup : IChangeGroup;
begin
  try
    if (FElementID > 0) then
    begin
      lChangeGroup := FAppModules.Changes.ChangeGroupWithID(FChangeGroupID); //FElementID
      if (lChangeGroup <> nil) then
      begin
        with ChangeListDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              EdtGroupName.FieldProperty.FieldName, Trim(EdtGroupName.Text), lMessage)) then
          begin
            EdtGroupName.FieldValidationError := lMessage;
            lChangeGroup.GroupName := Trim(EdtGroupName.Text);
            EdtGroupName.SetFieldValue(lChangeGroup.GroupName);
  //          DoContextValidation(dvtChanPropName);
          end
          else
            EdtGroupName.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.UpdateCreatedBy;
const OPNAME = 'TChangeListValidator.UpdateCreatedBy';
var
  lMessage    : string;
  lChangeList : IChangeList;
begin
  try
    if (FElementID > 0) then
    begin
      lChangeList := FAppModules.Changes.ChangeListWithID(FElementID);
      if (lChangeList <> nil) then
      begin
        with ChangeListDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              EdtPerson.FieldProperty.FieldName, Trim(EdtPerson.Text), lMessage)) then
          begin
            EdtPerson.FieldValidationError := lMessage;
            lChangeList.CreatedBy := Trim(EdtPerson.Text);
            EdtPerson.SetFieldValue(lChangeList.CreatedBy);
  //          DoContextValidation(dvtChanPropName);
          end
          else
            EdtPerson.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.UpdateDescription;
const OPNAME = 'TChangeListValidator.UpdateDescription';
var
  lMessage    : string;
  lChangeList : IChangeList;
begin
  try
    if (FElementID > 0) then
    begin
      lChangeList := FAppModules.Changes.ChangeListWithID(FElementID);
      if (lChangeList <> nil) then
      begin
        with ChangeListDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'ChangeListDescription', Trim(MmoDescription.Text), lMessage)) then
          begin
            MmoDescription.FieldValidationError := lMessage;
            lChangeList.Description   := Trim(MmoDescription.Text);
            MmoDescription.Lines.Text := lChangeList.Description;
//            DoContextValidation(dvtChanPropName);
          end
          else
            MmoDescription.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.ProcessMetaDataEvent : boolean;
const OPNAME = 'TChangeListValidator.ProcessMetaDataEvent';
var
  lFieldProperty : TAbstractFieldProperty;
  lKeyValues     : string;
  lRow           : integer;
  lFieldIndex    : string;
  lParamChange   : IParameterChange;
  lChangeList    : IChangeList;
begin
  Result := FALSE;
  try
    if (FPanel.Visible AND (FActiveControl <> nil) AND (FElementID <> 0)) then
    begin
      lChangeList := FAppModules.Changes.ChangeListWithID(FElementID);
      if (lChangeList <> nil) then
      begin
        with ChangeListDialog do
        begin
          if (FActiveControl = GrdParamChanges) then
          begin
            lRow           := GrdParamChanges.Row;
            lFieldIndex    := IntToStr(lRow);
            lFieldProperty := FAppModules.FieldProperties.FieldProperty('ChangeListItem');
            lParamChange   := lChangeList.ParamChangeByIndex(lRow-1);
            if (lFieldProperty <> nil) AND (lParamChange <> nil) then
            begin
              lKeyValues := lChangeList.GetKeyValues(lFieldProperty.FieldName, lFieldIndex);
              FAppModules.MetaData.ShowMetaData
                (lFieldProperty.FieldName, lKeyValues, lFieldIndex);
              RePopulateDataViewer;
              Result := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.CanCopyToCLipboard: boolean;
const OPNAME = 'TChangeListValidator.CanCopyToCLipboard';
begin
  Result := False;
  try
    Result := (ChangeListDialog.GrdParamChanges.RowCount > 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.CanExport: boolean;
const OPNAME = 'TChangeListValidator.CanExport';
begin
  Result := False;
  try
    Result := (ChangeListDialog.GrdParamChanges.RowCount > 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.CanPrint: boolean;
const OPNAME = 'TChangeListValidator.CanPrint';
begin
  Result := False;
  try
    Result := (ChangeListDialog.GrdParamChanges.RowCount > 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.DoCopyToCLipboard;
const OPNAME = 'TChangeListValidator.DoCopyToCLipboard';
begin
  try
    ChangeListDialog.GrdParamChanges.DoCopyToClipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.DoExport(AFileName: string = '');
const OPNAME = 'TChangeListValidator.DoExport';
begin
  try
    ChangeListDialog.GrdParamChanges.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.DoPrint;
const OPNAME = 'TChangeListValidator.DoPrint';
begin
  try
    ChangeListDialog.GrdParamChanges.DoPrint(FAppModules.Language.GetString('ChangeLists.ChangeList'));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeListValidator.TreeNodeType (ANode : TTreeNode) : TChangeElementType;
const OPNAME = 'TChangeListValidator.TreeNodeType';
begin
  Result := cetNone;
  try
    if (ANode <> nil) then
    begin
      case ANode.ImageIndex of
        0 : Result := cetChangeList;
        1 : Result := cetInactiveList;
        2 : Result := cetActiveList;
        3 : Result := cetInactiveGroup;
        4 : Result := cetActiveGroup;
        5 : Result := cetAlienList;
      else
        Result := cetNone;
      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.OnTrvChangesDragOver (Sender, Source : TObject;
                                                     X, Y           : Integer;
                                                     State          : TDragState;
                                                     var Accept     : Boolean);
const OPNAME = 'TChangeListValidator.OnTrvChangesDragOver';
begin
  try
    Accept := Source is TTreeView;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeListValidator.OnTrvChangesDragDrop (Sender, Source : TObject;
                                                     X, Y           : Integer);
const OPNAME = 'TChangeListValidator.OnTrvChangesDragDrop';
var
  lParentNode    : TTreeNode;
  lNewParentID   : integer;
  lErrorMsg      : string;
begin
  try
    with ChangeListDialog do
    begin
      if (FElementID > 0) then
      begin
        lNewParentID := 0;
        lParentNode  := TrvChangeGroup.GetNodeAt(X,Y);
        if (lParentNode <> nil) then
          lNewParentID := Integer(lParentNode.Data);

        if (FParentGroupID = lNewParentID) then
        begin
          { No move }
        end
        else
        if (lParentNode = nil) AND (FElementType in [cetChangeList, cetAlienList]) then
        begin
          { Cannot drag original list to empty space in treeview }
        end
        else
        if (lParentNode = nil) AND (FElementType in [cetInactiveList, cetActiveList]) then
        begin
          { Drag listnode in tree and drop on empty space = delete listnode from tree }
          FAppModules.Changes.DoDeleteChangeGroupElement
            (FParentGroupID, FElementID, FALSE);
        end
        else
        if (lNewParentID = FElementID) AND (FElementType in [cetInactiveGroup, cetActiveGroup]) then
        begin
          { Cannot add a ChangeGroup to itself }
        end
        else
        if (lParentNode <> nil) AND (TreeNodeType(lParentNode) in [cetInactiveList, cetActiveList]) then
        begin
          { Cannot dop element on listnode in tree }
        end
        else
        if (FAppModules.Changes.MayCreateNewChangeGroupElement
             (FParentGroupID, lNewParentID, FElementID, (FElementType in [cetInactiveGroup, cetActiveGroup]), lErrorMsg)) then
        begin
          if ((lParentNode = nil) AND (FElementType in [cetInactiveGroup, cetActiveGroup])) OR
             ((lParentNode <> nil) AND (TreeNodeType(lParentNode) in [cetInactiveGroup, cetActiveGroup])) then
          begin
            if (FParentGroupID >= 0) then
            begin
              FSystemFlag := TRUE;
              FAppModules.Changes.DoDeleteChangeGroupElement
                (FParentGroupID, FElementID, (FElementType in [cetInactiveGroup, cetActiveGroup]));
              FSystemFlag := FALSE;
            end;
            FAppModules.Changes.DoCreateNewChangeGroupElement
              (lNewParentID, FElementID, (FElementType in [cetInactiveGroup, cetActiveGroup]));
          end;
        end
        else
          ShowMessage(lErrorMsg);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TChangeListValidator.OnTreeViewClick(Sender: TObject);
const OPNAME = 'TChangeListValidator.OnTreeViewClick';
begin
  try
    PopulateControls;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeListValidator.GetSelectedNode(ATreeView: TAbstractTreeView): TTreeNode;
const OPNAME = 'TChangeListValidator.GetSelectedNode';
begin
  Result := nil;
  try
    if ATreeView <> nil then
      Result := ATreeView.Selected;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.

