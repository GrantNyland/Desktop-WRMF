//
//  UNIT      : Contains TNetworkVisualiserManager Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/02/24
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UNetworkVisualiserManager;

interface

uses
  Windows,
  Classes,
  VCL.ComCtrls,
  VCL.OleCtnrs,
  VCL.Menus,
  UAbstractObject,
  UVisioTabSheetManager,
  UGenericModelLinkClasses,
  UNetworkVisualiserData,
  UAbstractModelObjects,
  UNetworkVisualiserSheet,
  Registry,
  UPageViewer,
  Messages,
  VCL.ExtCtrls,
  UVNVEventHandler,
  UNetworkVisualiserDataLoadAgent;
type
  TVisioThread = class(TThread)
  private
    procedure FindVisioWindow;
    function GetVisioWindowHandle(APartOfText: String; AUseTitle: Boolean): HWND;
  protected
  public
    constructor Create;
    procedure Execute; override;
  end;
  TNetworkVisualiserManager = class(TVisioTabSheetManager)
  private
  protected
    //FVisioThread               : TVisioThread;
    FDrawingData               : TDrawingData;
    FDataLoadAgent             : TNetworkVisualiserDataLoadAgent;
    FVNVEventHandler           : TVNVEventHandler;
    FCopiedDrawing             : TDrawing;
    FCopiedDrawingGroup        : TDrawingGroup;
    FContextMenu               : TPopupMenu;
    FToggleDrawingReadOnlyFlag : TMenuItem;
    FCopyDrawingMenuItem       : TMenuItem;
    FPasteDrawingMenuItem      : TMenuItem;
    FCurrentGroupID            : integer;
    FCurrentDrawingID          : integer;
    FPageViewer                : TPageViewer;
    FSystemFlag                : boolean;
    //FWaitForVisioHandle        : THandle;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure PopulateDataObject;
    procedure PopulateTabSheet;
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode);
    procedure OnNewDrawingGroup;
    procedure OnDeleteDrawingGroup;
    procedure OnRenameDrawingGroup;
    procedure OnNewDrawing;
    procedure OnDeleteDrawing;
    procedure OnRenameDrawing;
    procedure OnEditDrawing;
    procedure OnViewDrawing;
    procedure OnCopyDrawing;
    procedure OnNameEdited (Sender: TObject; Node: TTreeNode; var AName : string);
    function UpdateDrawingName(ANode: TTreeNode; var ANewName: string): boolean;
    function UpdateGroupName(ANode: TTreeNode; var ANewName: string): boolean;
    function SetDrawingReadOnlyFlag(ANode: TTreeNode): boolean;
    function RemoveDrawingReadOnlyFlag(ANode: TTreeNode): boolean;
    procedure OnTreeViewDblClick(Sender: TObject);
    function NetworkVisualiserSheet: TNetworkVisualiserSheet;

    procedure OnToggleDrawingReadOnlyFlagClick(Sender: TObject);
    procedure OnCopyDrawingClick(Sender: TObject);
    procedure OnPasteDrawingClick(Sender: TObject);
    procedure OnPopupEvent(Sender: TObject);
    function InOCXMode:boolean;
    procedure DoBeforeDocumentClosed (ASender : TObject);
    procedure DoBeforeDocumentOpened (ASender : TObject);

    procedure OnVisioThreadTerminate(ASender: TObject);
    function UpdateRegistrySettings : boolean;
    procedure ClearRegistrySettings;
    function UpdateCOMIniFile: boolean;
  public
//    destructor Destroy; override;
    function Initialise: Boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext : TChangeContext;
                                  AFieldName,AOldValue,ANewValue: string): boolean; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    function HandleVNVEvent (AVisioApp       : IUnknown;
                             AVisioDoc       : IUnknown;
                             AVisioEventCode : integer;
                             ASourceObj      : IUnknown;
                             AEventID        : Integer;
                             AEventSeqNum    : Integer;
                             ASubjectObj     : IUnknown;
                             AMoreInfo       : OleVariant) : boolean; override;
    function ProcessVNVSpecial(const AParameter: WideString): boolean; override;
  end;

implementation

uses
  System.UITypes,
  VCL.StdCtrls,
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  VCL.Graphics,
  StrUtils,
  Contnrs,
  ShellApi,
  VCL.forms,
  INiFiles,
  UUtilities,
  VoaimsCom_TLB,
  Visio_TLB,
  VisOcx_TLB,
  UMainMenuEventType,
  VCL.Imaging.jpeg,
  UYieldModelDataObject,
  UAbstractComponent,
  UErrorHandlingOperations,
  UTabSheetManager;

function TNetworkVisualiserManager.InOCXMode: boolean;
const OPNAME = 'TNetworkVisualiserManager.InOCXMode';
begin
  Result := False;
  try
    Result := not Assigned(FAppModules.MainForm());
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.ClearRegistrySettings;
const OPNAME = 'TNetworkVisualiserManager.ClearRegistrySettings';
var
  LRegistry      : TRegistry;
  LRegistryKey   : String;
  LSharedMemLocation : string;
begin
  try
    LRegistry         := TRegistry.Create;
    try
      LRegistry.RootKey := HKEY_LOCAL_MACHINE;
      LRegistryKey      := 'SOFTWARE\Borland\Database Engine\Settings\SYSTEM\INIT';
      if LRegistry.OpenKey(LRegistryKey, False) then
      begin
        LSharedMemLocation := LRegistry.ReadString('SHAREDMEMLOCATION');
        if (LSharedMemLocation <> '') then
        begin
          LRegistry.WriteString('SHAREDMEMLOCATION','');
        end;
        LRegistry.CloseKey;
      end;
    finally
      LRegistry.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.CreateMemberObjects;
const OPNAME = 'TNetworkVisualiserManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet := nil;
    FCurrentGroupID     := -1;
    FCurrentDrawingID   := -1;
    FCopiedDrawing      := nil;
    FCopiedDrawingGroup := nil;
    FSystemFlag         := FALSE;
    if InOCXMode then
    begin
      FVNVEventHandler    := TVNVEventHandler.Create(FAppModules);
      //FVisioThread := TVisioThread.Create;
      //FVisioThread.OnTerminate := OnVisioThreadTerminate;
    end
    else
    begin
      FTabSheet           := TNetworkVisualiserSheet.Create(nil, FAppModules);
      FPageViewer         := TPageViewer.Create(FTabSheet,FAppModules);
      FPageViewer.Parent  := FTabSheet;

      FDrawingData        := TDrawingData.Create(FAppModules);
      FDataLoadAgent      := TNetworkVisualiserDataLoadAgent.Create(FAppModules);

      FContextMenu        := TPopupMenu.Create(nil);
      FContextMenu.OnPopup := OnPopupEvent;
      FToggleDrawingReadOnlyFlag := TMenuItem.Create(FContextMenu);
      FToggleDrawingReadOnlyFlag.Caption := FAppModules.Language.GetString('VNV.DrawingReadOnly');
      FToggleDrawingReadOnlyFlag.OnClick := OnToggleDrawingReadOnlyFlagClick;
      FContextMenu.Items.Add(FToggleDrawingReadOnlyFlag);

      FCopyDrawingMenuItem := TMenuItem.Create(FContextMenu);
      FCopyDrawingMenuItem.Caption := FAppModules.Language.GetString('VNV.CopyDrawing');
      FCopyDrawingMenuItem.OnClick := OnCopyDrawingClick;
      FContextMenu.Items.Add(FCopyDrawingMenuItem);

      FPasteDrawingMenuItem := TMenuItem.Create(FContextMenu);
      FPasteDrawingMenuItem.Caption := FAppModules.Language.GetString('VNV.PasteDrawing');
      FPasteDrawingMenuItem.OnClick := OnPasteDrawingClick;
      FContextMenu.Items.Add(FPasteDrawingMenuItem);

      NetworkVisualiserSheet.TreeView.PopupMenu := FContextMenu;

      NetworkVisualiserSheet.TreeView.OnChange   := DoTreeNodeHasChanged;
      NetworkVisualiserSheet.TreeView.OnEdited   := OnNameEdited;
      NetworkVisualiserSheet.TreeView.OnDblClick := OnTreeViewDblClick;

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.DestroyMemberObjects;
const OPNAME = 'TNetworkVisualiserManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    if InOCXMode then
    begin
      FreeAndNil(FVNVEventHandler);
      //FVisioThread.Terminate;
    end
    else
    begin
      FreeAndNil(FDrawingData);
      FreeAndNil(FDataLoadAgent);
      FreeAndNil(FContextMenu);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserManager.NetworkVisualiserSheet: TNetworkVisualiserSheet;
const OPNAME = 'TNetworkVisualiserManager.NetworkVisualiserSheet';
begin
  Result := nil;
  try
    if Assigned(FTabSheet)  then
      Result := TNetworkVisualiserSheet(FTabSheet);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserManager.Initialise: Boolean;
const OPNAME = 'TNetworkVisualiserManager.Initialise';
begin
  Result := inherited Initialise;
  try
    if Assigned(FDrawingData) then
      Result := FDrawingData.Initialise
    else
      Result := True;
    if NetworkVisualiserSheet <> nil then
      Result := NetworkVisualiserSheet.Initialise;

    //if FDataLoadAgent <> nil then
    //  FDataLoadAgent.ParentTab := NetworkVisualiserSheet;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserManager.LanguageHasChanged: boolean;
const OPNAME = 'TNetworkVisualiserManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned(FDrawingData) then
      Result := FDrawingData.LanguageHasChanged
    else
      Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.PopulateDataObject;
const OPNAME = 'TNetworkVisualiserManager.PopulateDataObject';
begin
  try
    if Assigned(FDataLoadAgent) and FDataLoadAgent.ConstructData(FDrawingData) then
      //NetworkVisualiserSheet.MenuItemManager.SetMenuNewDrawingGroup(msEnable);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.PopulateTabSheet;
const OPNAME = 'TNetworkVisualiserManager.PopulateTabSheet';
var
  LDrawingIndex      : integer;
  LDrawingGroupIndex : integer;
  LDrawingGroupList  : TDrawingGroupList;
  LDrawingGroup      : TDrawingGroup;
  LDrawingList       : TDrawingList;
  LDrawing           : TDrawing;
  LGroupNode         : TTreeNode;
  LDrawingNode       : TTreeNode;
  LTree              : TTreeView;
  LSelectedNode      : TTreeNode;
begin
  try
    if InOCXMode then Exit;
    LTree := NetworkVisualiserSheet.TreeView;
    FSystemFlag := TRUE;
    LTree.Items.Clear;
    LDrawingGroupList := FDrawingData.DrawingGroupList;
    LSelectedNode := nil;
    for LDrawingGroupIndex := 0 to LDrawingGroupList.DrawingGroupCount -1 do
    begin
      LDrawingGroup := LDrawingGroupList.DrawingGroupByIndex[LDrawingGroupIndex];
      LGroupNode    := LTree.Items.AddObjectFirst(nil, LDrawingGroup.DrawingGroupName, LDrawingGroup);
      if (LDrawingGroup.DrawingGroupID = FCurrentGroupID) then
        LSelectedNode := LGroupNode;
      LDrawingList  := LDrawingGroup.DrawingList;
      for LDrawingIndex := 0 to LDrawingList.DrawingCount -1 do
      begin
        LDrawing := LDrawingList.DrawingByIndex[LDrawingIndex];
        LDrawingNode := LTree.Items.AddChildObject(LGroupNode, LDrawing.DrawingName, LDrawing);
        if (LDrawingGroup.DrawingGroupID = FCurrentGroupID) AND
           (LDrawing.DrawingID = FCurrentDrawingID) then
          LSelectedNode := LDrawingNode;
      end;
    end;
    LTree.AlphaSort(False);
    LTree.ReadOnly := False;
    if (LSelectedNode = nil) AND (LTree.Items.Count > 0) then
      LSelectedNode := LTree.Items[0];
    FSystemFlag := FALSE;
    LTree.Selected := LSelectedNode;
    if (LTree.Selected = nil) then
      LTree.OnChange(Self, nil);
    //NetworkVisualiserSheet.MenuItemManager.SetMenuNewDrawingGroup(msEnable);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserManager.StudyHasChanged: boolean;
const OPNAME = 'TNetworkVisualiserManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if not InOCXMode then
    begin
      FDrawingData.StudyHasChanged ;
      PopulateDataObject;
      PopulateTabSheet;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserManager.StudyDataHasChanged (AContext   : TChangeContext;
                                                        AFieldName : string;
                                                        AOldValue  : string;
                                                        ANewValue  : string): boolean;
const OPNAME = 'TNetworkVisualiserManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Assigned(FVNVEventHandler) then
      Result := Result and FVNVEventHandler.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure TNetworkVisualiserManager.DoTreeNodeHasChanged(ASender: TObject;ANode: TTreeNode);
const OPNAME = 'TNetworkVisualiserManager.DoTreeNodeHasChanged';
Var
  LDrawingID    : integer;
  LGroupID      : integer;
  LFileName     : string;
begin
  try
    FPageViewer.Visible := False;
    if (NOT FSystemFlag) then
    begin
      FPageViewer.ClearPages;
      NetworkVisualiserSheet.HintDisplay.Panels[1].Text := '';
      if (ANode = nil) then
      begin
        NetworkVisualiserSheet.MenuItemManager.SetMenuNewDrawing(msEnable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuDeleteDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuRenameDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuViewDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuCopyDrawing(msDisable);
        FCurrentGroupID   := -1;
        FCurrentDrawingID := -1;
      end
      else
      if ANode.Level = 0 then
      begin
        NetworkVisualiserSheet.MenuItemManager.SetMenuNewDrawing(msEnable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuDeleteDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuRenameDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuViewDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuCopyDrawing(msDisable);
        FCurrentGroupID   := TDrawingGroup(ANode.Data).DrawingGroupID;
        FCurrentDrawingID := -1;
      end
      else
      begin
        NetworkVisualiserSheet.MenuItemManager.SetMenuNewDrawing(msDisable);
        LFileName := FDataLoadAgent.GetVSDFileName(ANode.Parent.Text,ANode.Text);
        if not FileExists(LFileName) then
        begin
          NetworkVisualiserSheet.MenuItemManager.SetMenuDeleteDrawing(msDisable);
          NetworkVisualiserSheet.MenuItemManager.SetMenuRenameDrawing(msDisable);
          NetworkVisualiserSheet.MenuItemManager.SetMenuViewDrawing(msDisable);
          NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msDisable);
          NetworkVisualiserSheet.MenuItemManager.SetMenuCopyDrawing(msDisable);
          NetworkVisualiserSheet.HintDisplay.Panels[1].Text := 'File ('+LFileName+') does not exist.';
        end
        else
        begin
          NetworkVisualiserSheet.MenuItemManager.SetMenuNewDrawing(msDisable);
          NetworkVisualiserSheet.MenuItemManager.SetMenuDeleteDrawing(msEnable);
          NetworkVisualiserSheet.MenuItemManager.SetMenuRenameDrawing(msEnable);
          NetworkVisualiserSheet.MenuItemManager.SetMenuViewDrawing(msEnable);
          NetworkVisualiserSheet.MenuItemManager.SetMenuCopyDrawing(msEnable);
          FCurrentGroupID   := TDrawing(ANode.Data).DrawingGroupID;
          FCurrentDrawingID := TDrawing(ANode.Data).DrawingID;
          LDrawingID    := TDrawing(ANode.Data).DrawingID;
          LGroupID      := TDrawing(ANode.Data).DrawingGroupID;
          if FDataLoadAgent.IsDrawingReadOnly(LGroupID,LDrawingID) then
            NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msDisable)
          else
            NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msEnable);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{$WARN SYMBOL_PLATFORM ON}

function TNetworkVisualiserManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'TNetworkVisualiserManager.DoCustomTabSheetEvent';
var
  lMsg       : string;
begin
  Result := inherited DoCustomTabSheetEvent(ACustomModelEvent);
  try
    if not Result then
    begin
      Result := True;
      case ACustomModelEvent.Action of
        meNewDrawingGroup    : OnNewDrawingGroup;
        meDeleteDrawingGroup : OnDeleteDrawingGroup;
        meRenameDrawingGroup : OnRenameDrawingGroup;
        meNewDrawing         : OnNewDrawing;
        meViewDrawing        : OnViewDrawing;
        meCopyDrawing        : OnCopyDrawing;
        meDeleteDrawing      :
        	begin
            lMsg := FAppModules.Language.GetString('VNV.ConfirmDeleteDrawing');
            if MessageDlg(lMsg, mtConfirmation, [mbYes,mbCancel], 0) = mrCancel then
              exit;
            OnDeleteDrawing;
          end;
        meRenameDrawing      : OnRenameDrawing;
        meEditDrawing        : OnEditDrawing;
        else
          Result := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.OnNewDrawingGroup;
const OPNAME = 'TNetworkVisualiserManager.OnNewDrawingGroup';
var
  LGroupName    : string;
  LDrawingGroup : TDrawingGroup;
begin
  try
    FPageViewer.Visible := False;
    {if InputQuery(FAppModules.Language.GetString('VNV.EnterGroupName'),
                  FAppModules.Language.GetString('VNV.GroupName'), LGroupName) then
    begin
    }
    //if FDataLoadAgent <> nil then
    // FDataLoadAgent.ParentTab := NetworkVisualiserSheet;

      LGroupName := FAppModules.StudyArea.ScenarioCode;
      LGroupName := Trim(LGroupName);
      if (LGroupName = '') then
        ShowMessage(FAppModules.Language.GetString('VNV.GroupNameEmpty'))
      else
      begin
        if FDrawingData.DrawingGroupExist(LGroupName) then
          LGroupName := 'New '+LGroupName;
          //ShowMessage(FAppModules.Language.GetString('VNV.GroupNameExists'))

        //else
        //begin
          LDrawingGroup := FDrawingData.DrawingGroupList.CreateDrawingGroup(LGroupName);
          if (LDrawingGroup <> nil) then
          begin
            if FDataLoadAgent.CreateDrawingGroup(LDrawingGroup) then
            begin
              FCurrentGroupID := LDrawingGroup.DrawingGroupID;
              PopulateTabSheet;
            end
            else
            begin
              LDrawingGroup := nil;
              if (LDrawingGroup = nil) then
                FDrawingData.DrawingGroupList.DeleteDrawingGroup(LGroupName);
            end;
          end;
        //end;
      end;
    //end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.OnRenameDrawingGroup;
const OPNAME = 'TNetworkVisualiserManager.OnRenameDrawingGroup';
var
  lNewName     : string;
  lOldName     : string;
  lGroup       : TDrawingGroup;
  lNode        : TTreeNode;
  lGroupID     : integer;
  lDrawing     : TDrawing;
  lOldFileName : string;
  lNewFileName : string;
  lIndex       : integer;
begin
  try
    FPageViewer.Visible := False;
    lNode := NetworkVisualiserSheet.TreeView.Selected;
    if (lNode = nil) OR (lNode.Level <> 0) then
      ShowMessage(FAppModules.Language.GetString('VNV.SelectGroup'))
    else
    begin
      if (lNode.Data <> nil) then
      begin
        lGroup   := TDrawingGroup(LNode.Data);
        lOldName := lGroup.DrawingGroupName;
        if InputQuery(FAppModules.Language.GetString('VNV.EnterNewGroupName'),
                      FAppModules.Language.GetString('VNV.NewGroupName'), lNewName) then
        begin
          lNewName := Trim(lNewName);
          if (lNewName = '') then
            ShowMessage(FAppModules.Language.GetString('VNV.GroupNameEmpty'))
          else
          begin
            if FDrawingData.DrawingGroupExist(lNewName) then
              ShowMessage(FAppModules.Language.GetString('VNV.GroupNameExists'))
            else
            begin
              lGroupID := lGroup.DrawingGroupID;
              if FDataLoadAgent.UpdateGroupName(lGroupID, lNewName) then
              begin
                lGroup.DrawingGroupName := lNewName;
                lNode.Text              := lNewName;
                for lIndex := 0 to lGroup.DrawingList.DrawingCount - 1 do
                begin
                  lDrawing := lGroup.DrawingList.DrawingByIndex[lIndex];
                  lOldFileName := FDataLoadAgent.GetVSDFileName(lOldName, lDrawing.DrawingName);
                  lNewFileName := FDataLoadAgent.GetVSDFileName(lNewName, lDrawing.DrawingName);
                  RenameFile(lOldFileName, lNewFileName);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserManager.UpdateGroupName(ANode: TTreeNode; var ANewName: string): boolean;
const OPNAME = 'TNetworkVisualiserManager.UpdateGroupName';
var
  LGroupID     : integer;
  lDrawing     : TDrawing;
  lGroup       : TDrawingGroup;
  lOldFileName : string;
  lNewFileName : string;
  lIndex       : integer;
  lOldName     : string;
begin
  Result := False;
  try
    if (ANode.Level = 0) then
    begin
      if FDrawingData.DrawingGroupExist(ANewName) then
      begin
        ShowMessage(FAppModules.Language.GetString('VNV.GroupNameExists'));
      end
      else
      begin
        lGroup     := TDrawingGroup(ANode.Data);
        LGroupID   := lGroup.DrawingGroupID;
        if FDataLoadAgent.UpdateGroupName(LGroupID,ANewName) then
        begin
          lOldName := lGroup.DrawingGroupName;
          lGroup.DrawingGroupName := ANewName;
          for lIndex := 0 to lGroup.DrawingList.DrawingCount - 1 do
          begin
            lDrawing := lGroup.DrawingList.DrawingByIndex[lIndex];
            lOldFileName := FDataLoadAgent.GetVSDFileName(lOldName, lDrawing.DrawingName);
            lNewFileName := FDataLoadAgent.GetVSDFileName(ANewName, lDrawing.DrawingName);
            RenameFile(lOldFileName, lNewFileName);
          end;
          Result := True;
        end
        else
          ANewName := TDrawingGroup(ANode.Data).DrawingGroupName;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.OnDeleteDrawingGroup;
const OPNAME = 'TNetworkVisualiserManager.OnDeleteDrawingGroup';
var
  LNode         : TTreeNode;
  LRet          : integer;
  LDrawingGroup : TDrawingGroup;
  LDrawing      : TDrawing;
  LIndex        : integer;
begin
  try
    FPageViewer.Visible := False;
    LNode := NetworkVisualiserSheet.TreeView.Selected;
    if (LNode = nil) OR (LNode.Level <> 0) then
      ShowMessage(FAppModules.Language.GetString('VNV.SelectGroup'))
    else
    begin
      LDrawingGroup := TDrawingGroup(LNode.Data);
      if (LDrawingGroup.DrawingList.DrawingCount > 0) then
      begin
        LRet := MessageDlg(FAppModules.Language.GetString('VNV.ConfirmDeleteGroup'), mtConfirmation, [mbYes,mbCancel], 0);
        if LRet = mrCancel then
          exit;
        for LIndex := (LDrawingGroup.DrawingList.DrawingCount -1) downto 0  do
        begin
          LDrawing := LDrawingGroup.DrawingList.DrawingByIndex[LIndex];
          if FDataLoadAgent.DeleteDrawing(LDrawingGroup, LDrawing) then
            LDrawingGroup.DrawingList.DeleteDrawing(LDrawing.DrawingName);
        end;
      end;

      if FDataLoadAgent.DeleteDrawingGroup(LDrawingGroup.DrawingGroupID) then
      begin
        lIndex := LNode.Index;
        FDrawingData.DrawingGroupList.DeleteDrawingGroup(LDrawingGroup.DrawingGroupName);

        if (FDrawingData.DrawingGroupList.DrawingGroupCount = 0) then
          FCurrentGroupID := -1
        else
        begin
          if (lIndex >= 0) AND (lIndex < FDrawingData.DrawingGroupList.DrawingGroupCount) then
          begin
            LDrawingGroup := FDrawingData.DrawingGroupList.DrawingGroupByIndex[lIndex];
            FCurrentGroupID := LDrawingGroup.DrawingGroupID;
          end
          else
          begin
            lIndex := FDrawingData.DrawingGroupList.DrawingGroupCount - 1;
            LDrawingGroup := FDrawingData.DrawingGroupList.DrawingGroupByIndex[lIndex];
            FCurrentGroupID := LDrawingGroup.DrawingGroupID;
          end;
        end;
        PopulateTabSheet;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserManager.UpdateDrawingName(ANode: TTreeNode; var ANewName: string): boolean;
const OPNAME = 'TNetworkVisualiserManager.UpdateDrawingName';
var
  LDrawingID   : integer;
  lDrawing     : TDrawing;
  lGroup       : TDrawingGroup;
  LGroupID     : integer;
  lOldFileName : string;
  lNewFileName : string;
begin
  Result := False;
  try
    if (ANode.Level = 1) then
    begin
      if FDrawingData.DrawingExist(ANode.Parent.Text, ANewName) then
      begin
        ANewName := TDrawing(ANode.Data).DrawingName;
        ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameExistsInGroup'));
      end
      else
      begin
        lGroup     := TDrawingGroup(ANode.Parent.Data);
        lDrawing   := TDrawing(ANode.Data);
        LGroupID   := lGroup.DrawingGroupID;
        LDrawingID := LDrawing.DrawingID;
        if FDataLoadAgent.UpdateDrawingName(LGroupID, LDrawingID, ANewName) then
        begin
          lOldFileName := FDataLoadAgent.GetVSDFileName(lGroup.DrawingGroupName, lDrawing.DrawingName);
          lNewFileName := FDataLoadAgent.GetVSDFileName(lGroup.DrawingGroupName, ANewName);
          RenameFile(lOldFileName, lNewFileName);
          lDrawing.DrawingName := ANewName;
          Result := True;
        end
        else
          ANewName := TDrawing(ANode.Data).DrawingName;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.OnRenameDrawing;
const OPNAME = 'TNetworkVisualiserManager.OnRenameDrawing';
var
  lNode         : TTreeNode;
  lDrawingID    : integer;
  lDrawing      : TDrawing;
  lGroup        : TDrawingGroup;
  lGroupID      : integer;
  lOldFileName  : string;
  lNewFileName  : string;
  lNewName      : string;
  lGroupName    : string;
begin
  try
    FPageViewer.Visible := False;
    lNode := NetworkVisualiserSheet.TreeView.Selected;
    if (lNode = nil) OR (lNode.Level <> 1) then
      ShowMessage(FAppModules.Language.GetString('VNV.SelectDrawing'))
    else
    begin
      if (lNode.Data <> nil) then
      begin
        lGroup     := TDrawingGroup(LNode.Parent.Data);
        lGroupName := lGroup.DrawingGroupName;
        if InputQuery(FAppModules.Language.GetString('VNV.EnterNewDrawingName'),
                      FAppModules.Language.GetString('VNV.NewDrawingName'), lNewName) then
        begin
          lNewName := Trim(lNewName);
          if (lNewName = '') then
            ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameEmpty'))
          else
          begin
            if FDrawingData.DrawingExist(lGroupName, lNewName) then
              ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameExists'))
            else
            begin
              lDrawing   := TDrawing(lNode.Data);
              lGroupID   := lGroup.DrawingGroupID;
              lDrawingID := LDrawing.DrawingID;
              if FDataLoadAgent.UpdateDrawingName(lGroupID, lDrawingID, lNewName) then
              begin
                lOldFileName := FDataLoadAgent.GetVSDFileName(lGroupName, lDrawing.DrawingName);
                lNewFileName := FDataLoadAgent.GetVSDFileName(lGroupName, lNewName);
                RenameFile(lOldFileName, lNewFileName);
                lDrawing.DrawingName := lNewName;
                lNode.Text           := lNewName;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.OnNewDrawing;
const OPNAME = 'TNetworkVisualiserManager.OnNewDrawing';
var
  LGroupName     : string;
  LDrawingName   : string;
  LNode          : TTreeNode;
  LDrawingGroup  : TDrawingGroup;
  LDrawing       : TDrawing;
  LGISDrawing    : boolean;
  LMesgDlgResult : Word;
  LMsg           : string;
begin
  try
    FPageViewer.Visible := False;
    LNode := NetworkVisualiserSheet.TreeView.Selected;
    if (LNode = nil) {OR (LNode.Level <> 0)} then
    begin
      OnNewDrawingGroup;
      LNode := NetworkVisualiserSheet.TreeView.Selected;
    end;
    if (LNode.Data <> nil) then
    begin
      LMsg := FAppModules.Language.GetString('VNV.DrawingGISConfirmation');
      LMesgDlgResult := WRMFMessageDialog(LMsg,mtConfirmation,mbYesNoCancel,[
                                            FAppModules.Language.GetString('LabelText.No'),
                                            FAppModules.Language.GetString('LabelText.Yes')]);
      if (LMesgDlgResult = mrCancel) then Exit;
      LGISDrawing := (LMesgDlgResult = mrNo);


      LDrawingGroup := TDrawingGroup(LNode.Data);
      LGroupName := LDrawingGroup.DrawingGroupName;
      if InputQuery(FAppModules.Language.GetString('VNV.EnterDrawingName'),
                    FAppModules.Language.GetString('VNV.DrawingName'),LDrawingName) then
      begin
        LDrawingName := Trim(LDrawingName);
        if (LDrawingName = '') then
          ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameEmpty'))
        else
        begin
          if FDrawingData.DrawingExist(LGroupName,LDrawingName) then
           ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameExists'))
          else
          begin
            LDrawing := LDrawingGroup.DrawingList.CreateDrawing(LDrawingName);
            if (LDrawing <> nil) then
            begin
              LDrawing.GISMode  := LGISDrawing;
              if FDataLoadAgent.CreateDrawing(LDrawingGroup,LDrawing) then
              begin
                FCurrentDrawingID := LDrawing.DrawingID;
                PopulateTabSheet;
              end
              else
              begin
               LDrawing := nil;
               if (LDrawing = nil) then
                 LDrawingGroup.DrawingList.DeleteDrawing(LDrawingName);
              end;
            end;
          end
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.OnDeleteDrawing;
const OPNAME = 'TNetworkVisualiserManager.OnDeleteDrawing';
var
  LNode         : TTreeNode;
  LDrawingGroup : TDrawingGroup;
  LDrawing      : TDrawing;
  lDrawingName  : string;
  lIndex        : integer;
begin
  try
    FPageViewer.Visible := False;
    LNode := NetworkVisualiserSheet.TreeView.Selected;
    if (LNode = nil) OR (LNode.Level <> 1) then
      ShowMessage(FAppModules.Language.GetString('VNV.SelectDrawing'))
    else
    begin
      if (LNode.Data <> nil) then
      begin
        lIndex := LNode.Index;
        LDrawingGroup := TDrawingGroup(LNode.Parent.Data);
        LDrawing      := TDrawing(LNode.Data);
        lDrawingName  := LDrawing.DrawingName;
        if FDataLoadAgent.DeleteDrawing(LDrawingGroup, LDrawing) then
        begin
          LDrawingGroup.DrawingList.DeleteDrawing(lDrawingName);
          if (LDrawingGroup.DrawingList.DrawingCount = 0) then
            FCurrentDrawingID := -1
          else
          begin
            if (lIndex >= 0) AND (lIndex < LDrawingGroup.DrawingList.DrawingCount) then
            begin
              LDrawingGroup := TDrawingGroup(LNode.Parent.Data);
              LDrawing      := LDrawingGroup.DrawingList.DrawingByIndex[lIndex];
              FCurrentDrawingID := LDrawing.DrawingID;
            end
            else
            begin
              lIndex := LDrawingGroup.DrawingList.DrawingCount - 1;
              LDrawing := TDrawingGroup(LNode.Parent.Data).DrawingList.DrawingByIndex[lIndex];
              FCurrentDrawingID := LDrawing.DrawingID;
            end;
          end;
          PopulateTabSheet;
        end;
        FPageViewer.ClearPages;
       end;
    end;
//   PopulateTabSheet;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure TNetworkVisualiserManager.OnViewDrawing;
const OPNAME = 'TNetworkVisualiserManager.OnViewDrawing';
var
  LGroupName   : string;
  LDrawingName : string;
  LDrawingID   : integer;
  LGroupID     : integer;
  LFileName    : string;
  LNode        : TTreeNode;
  LDrawingApp  : TDrawingControl;
  LIndex       : integer;
  LTempStr     : string;
  LImageViewer : TImageViewer;
begin
  try
    FPageViewer.Visible := False;
  	LNode := NetworkVisualiserSheet.TreeView.Selected;
    case LNode.Level of
      0:
      begin
        //NetworkVisualiserSheet.MenuItemManager.SetMenuDeleteDrawingGroup(msEnable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuNewDrawing(msEnable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuDeleteDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msDisable);
      end;
      1:
      begin
        //NetworkVisualiserSheet.MenuItemManager.SetMenuDeleteDrawingGroup(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuNewDrawing(msDisable);
        NetworkVisualiserSheet.MenuItemManager.SetMenuDeleteDrawing(msEnable);
        LGroupName    := LNode.Parent.Text;
        LDrawingName  := LNode.Text;
        LDrawingID    := TDrawing(LNode.Data).DrawingID;
        LGroupID      := TDrawing(LNode.Data).DrawingGroupID;
        if FDataLoadAgent.IsDrawingReadOnly(LGroupID,LDrawingID) then
        	NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msDisable)
        else
	        NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msEnable);
        LFileName := FDataLoadAgent.GetVSDFileName(LGroupName, LDrawingName);
        if FileExists(LFileName) then
        begin
          FPageViewer.Visible := True;

          // Generate the JPEG files from the VSD file
          LDrawingApp := TDrawingControl.Create(nil);
          try
            LDrawingApp.Visible := false;
            LDrawingApp.Parent  := NetworkVisualiserSheet;
            LDrawingApp.Src     := LFileName;
            FPageViewer.ImageCount := LDrawingApp.Document.Pages.Count;
            for LIndex := 1 to LDrawingApp.Document.Pages.Count do
            begin
              LImageViewer := FPageViewer.ImageByIndex[LIndex-1];
              LImageViewer.Caption := LDrawingApp.Document.Pages.Item[LIndex].Name;

              LTempStr := ChangeFileExt(LFileName,'.JPG');
              LDrawingApp.Document.Pages.Item[LIndex].Export(LTempStr);
              LImageViewer.Image.Picture.LoadFromFile(LTempStr);
              DeleteFile(LTempStr);
            end;
          finally
            LDrawingApp.Src := '';
            LDrawingApp.Parent := nil;
            LDrawingApp.Free;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.OnEditDrawing;
const OPNAME = 'TNetworkVisualiserManager.OnEditDrawing';
var
  LExecInfo       : TShellExecuteInfo; //SHELLEXECUTEINFOA;
	lFileName       : string;
  LNode           : TTreeNode;
  lGroupName      : string;
  lDrawingName    : string;
  LEventHandle    : THandle;
  lMsg            : string;
  LScenarioLocked : boolean;
  LModelCode      : string;
  LStudyCode      : string;
  LSubArea        : string;
  LScenarioCode   : string;
  LExitLoop       : boolean;
  LFileAttr       : integer;
begin
  try
    FPageViewer.Visible := False;
    LNode := NetworkVisualiserSheet.TreeView.Selected;
    if LNode = nil then
  	  exit;
    FPageViewer.ClearPages;
    if LNode.Level = 1 then
    begin
      lGroupName    := LNode.Parent.Text;
      lDrawingName  := LNode.Text;
      Application.ProcessMessages;
      lFileName := FDataLoadAgent.GetVSDFileName(lGroupName, lDrawingName);
      if (NOT FileExists(lFileName))then
      begin
        lMsg := lFileName + FAppModules.Language.GetString('VisioNetwork.MsgNotExist');
        MessageDlg(lFileName, mtError, [mbOK], 0);
      end
      else
      begin
        LFileAttr := FileGetAttr(lFileName);
        if (LFileAttr and faReadOnly) = faReadOnly then
          FileSetAttr(lFileName, LFileAttr and not faReadOnly);

        if UpdateCOMIniFile then
        begin
          LScenarioLocked  := not FAppModules.StudyArea.ScenarioLocked;
          LModelCode       := FAppModules.StudyArea.ModelCode;
          LStudyCode       := FAppModules.StudyArea.StudyAreaCode;
          LSubArea         := FAppModules.StudyArea.SubAreaCode;
          LScenarioCode    := FAppModules.StudyArea.ScenarioCode;
          if LScenarioLocked  then
            FAppModules.ScenarioLockManager.ReleaseLock;
          try
            lMsg := FAppModules.Language.GetString('VNV.AttemptToStartVisio');
            MessageDlg(lMsg, mtInformation, [mbOK], 0);
            UpdateRegistrySettings;
            FillChar(LExecInfo, SizeOf(LExecInfo), 0);
            LExecInfo.cbSize := SizeOf(TShellExecuteInfo);
            //ZeroMemory(@LExecInfo,sizeof(LExecInfo));
            //LExecInfo.cbSize := sizeof(LExecInfo);
            LExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
            LExecInfo.lpVerb := 'open';
            LExecInfo.lpFile := PChar(lFileName); //PAnsiChar(AnsiString(lFileName));
            LExecInfo.nShow := SW_SHOWMAXIMIZED;
            if not (ShellExecuteEx(@LExecInfo)) then //if (ShellExecuteEx(@LExecInfo) = false)then
            begin
              MessageDlg(FAppModules.Language.GetString('VNV.FailedToEditDiagram'), mtError, [mbOK], 0);
              Exit;
            end;

            LExitLoop := False;
            CreateEvent(nil, TRUE, False, 'EditingFinished');
            repeat
              if TAbstractMainForm(FAppModules.MainForm.MainForm).Closing then
                LExitLoop := True;

              if not LExitLoop then
              begin
                if (WaitForSingleObject(LExecInfo.hProcess,100) = WAIT_OBJECT_0) then
                  LExitLoop := True;
              end;


              LEventHandle := OpenEvent(EVENT_ALL_ACCESS,True,'EditingFinished');
              if (LEventHandle = 0) then
                LExitLoop := True
              else
              begin
                if (WaitForSingleObject(LEventHandle, 100) = WAIT_OBJECT_0) then
                begin
                  LExitLoop := True;
                end;
              end;

              if LExitLoop then
              begin
                if (LEventHandle <> 0) then
                begin
                  ResetEvent(LEventHandle);
                  CloseHandle(LEventHandle);
                end;
              end;

              if not LExitLoop then
                Application.ProcessMessages;
            until (LExitLoop = True);

            //UUtilities.ExecAndWait(lFileName,'')
          finally
            if not TAbstractMainForm(FAppModules.MainForm.MainForm).Closing then
            begin
              ClearRegistrySettings;
              if LScenarioLocked  then
              begin
                FAppModules.ScenarioLockManager.RequestScenarioUnlock(LModelCode,LStudyCode,LSubArea,LScenarioCode);
                FAppModules.ScenarioLockManager.RequestLock;
              end;
              FAppModules.Model.ProcessEvent(CmeReloadModelData,nil);
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{$WARN SYMBOL_PLATFORM ON}

procedure TNetworkVisualiserManager.OnNameEdited (Sender: TObject; Node: TTreeNode; var AName : string);
const OPNAME = 'TNetworkVisualiserManager.OnNameEdited';
begin
  try
    case Node.Level of
      0: UpdateGroupName(Node, AName);
      1: UpdateDrawingName(Node, AName)
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserManager.SetDrawingReadOnlyFlag(ANode: TTreeNode): boolean;
const OPNAME = 'TNetworkVisualiserManager.SetDrawingReadOnlyFlag';
var
  LDrawingID : integer;
  LGroupID   : integer;
begin
  Result := False;
  try
    if (ANode.Level = 1) then
    begin
      LGroupID   := TDrawing(ANode.Data).DrawingGroupID;
      LDrawingID := TDrawing(ANode.Data).DrawingID;
      FDataLoadAgent.SetDrawingReadOnlyFlag(LGroupID,LDrawingID)
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TNetworkVisualiserManager.RemoveDrawingReadOnlyFlag(ANode: TTreeNode): boolean;
const OPNAME = 'TNetworkVisualiserManager.RemoveDrawingReadOnlyFlag';
var
  LDrawingID : integer;
  LGroupID   : integer;
begin
  Result := False;
  try
    if (ANode.Level = 1) then
    begin
      LGroupID   := TDrawing(ANode.Data).DrawingGroupID;
      LDrawingID := TDrawing(ANode.Data).DrawingID;
      FDataLoadAgent.RemoveDrawingReadOnlyFlag(LGroupID,LDrawingID)
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TNetworkVisualiserManager.OnToggleDrawingReadOnlyFlagClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserManager.OnToggleDrawingReadOnlyFlagClick';
var
 	LNode      : TTreeNode;
  LGroupID   : integer;
  LDrawingID : integer;
begin
	try
    LNode := NetworkVisualiserSheet.TreeView.Selected;
    if LNode = nil then // No selection
      exit;
    if LNode.Level <> 1 then
      exit;
    LGroupID := TDrawing(LNode.Data).DrawingGroupID;
    LDrawingID := TDrawing(LNode.Data).DrawingID;
    if not FToggleDrawingReadOnlyFlag.Checked then
    begin
      FDataLoadAgent.SetDrawingReadOnlyFlag(LGroupID,LDrawingID);
      NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msDisable);
    end
    else
    begin
      FDataLoadAgent.RemoveDrawingReadOnlyFlag(LGroupID,LDrawingID);
      NetworkVisualiserSheet.MenuItemManager.SetMenuEditDrawing(msEnable);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserManager.OnCopyDrawingClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserManager.OnCopyDrawingClick';
var
  LNode : TTreeNode;
begin
  try
    LNode := NetworkVisualiserSheet.TreeView.Selected;
    if (LNode <> nil) and (LNode.Level = 1) then
    begin
      FCopiedDrawing := TDrawing(LNode.Data);
      FCopiedDrawingGroup := TDrawingGroup(LNode.Parent.Data);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure TNetworkVisualiserManager.OnTreeViewDblClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserManager.OnTreeViewDblClick';
begin
	try
    OnViewDrawing
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{$WARN SYMBOL_PLATFORM ON}

procedure TNetworkVisualiserManager.OnPasteDrawingClick(Sender: TObject);
const OPNAME = 'TNetworkVisualiserManager.OnPasteDrawingClick';
var
  LNode         : TTreeNode;
  LDrawing      : TDrawing;
  LDrawingGroup : TDrawingGroup;
  LDrawingName  : string;
  lIndex        : integer;
begin
	try
    LNode := NetworkVisualiserSheet.TreeView.Selected;
    if (LNode = nil) or (LNode.Level <> 0) then
      exit;
    if (FCopiedDrawing <> nil) then
    begin
      LDrawingName := 'Copy of ' + FCopiedDrawing.DrawingName;
      lIndex := 2;
      while FDrawingData.DrawingExist(LNode.Text,LDrawingName) do
      begin
        LDrawingName := 'Copy(' + IntToStr(lIndex) + ') of ' + FCopiedDrawing.DrawingName;
        inc(lIndex);
      end;
      if InputQuery(FAppModules.Language.GetString('VNV.NetworkVisualiser'),
                    FAppModules.Language.GetString('VNV.EnterNewDrawingName'),LDrawingName) then
      begin;
        LDrawingGroup := TDrawingGroup(LNode.Data);
        LDrawing      := LDrawingGroup.DrawingList.CreateDrawing(LDrawingName);
        FDataLoadAgent.CopyDrawing(LDrawingGroup, LDrawing, FCopiedDrawingGroup, FCopiedDrawing);
        FCurrentGroupID   := LDrawingGroup.DrawingGroupID;
        FCurrentDrawingID := LDrawing.DrawingID;
        PopulateTabSheet;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserManager.OnPopupEvent(Sender: TObject);
const OPNAME = 'TNetworkVisualiserManager.OnPopupEvent';
var
  LNode      : TTreeNode;
  lIndex     : integer;
  LDrawingID : integer;
  LGroupID   : integer;
begin
	try
    LNode := NetworkVisualiserSheet.TreeView.Selected;
    if LNode = nil then
    begin
      for lIndex := 0 to FContextMenu.Items.Count - 1  do
        FContextMenu.Items.Items[lIndex].Enabled := false;
      exit;
    end;
    if LNode.Level = 0 then
    begin
      FToggleDrawingReadOnlyFlag.Enabled := false;
      FCopyDrawingMenuItem.Enabled := false;
      if (FCopiedDrawing <> nil) then
        FPasteDrawingMenuItem.Enabled := TRUE
      else
        FPasteDrawingMenuItem.Enabled := FALSE;
    end
    else
    begin
      FToggleDrawingReadOnlyFlag.Enabled := true;
      FCopyDrawingMenuItem.Enabled := true;
      FPasteDrawingMenuItem.Enabled := false;
      LDrawingID := TDRawing(LNode.Data).DrawingID;
      LGroupID := TDrawing(LNode.Data).DrawingGroupID;
      if FDataLoadAgent.IsDrawingReadOnly(LGroupID,LDrawingID) then
        FToggleDrawingReadOnlyFlag.Checked := true
      else
        FToggleDrawingReadOnlyFlag.Checked := false;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserManager.OnCopyDrawing;
const OPNAME = 'TNetworkVisualiserManager.OnCopyDrawing';
var
  LGroupName    : string;
  LDrawingName  : string;
  LNode         : TTreeNode;
  LDrawingGroup : TDrawingGroup;
  LOldDrawing   : TDrawing;
  LNewDrawing   : TDrawing;
begin
  try
    FPageViewer.Visible := False;
    LNode := NetworkVisualiserSheet.TreeView.Selected;
    if (LNode.Level <> 1) then
       ShowMessage(FAppModules.Language.GetString('VNV.SelectDrawing'))
    else
    begin
      if (LNode.Data <> nil) then
      begin
        LOldDrawing := TDrawing(LNode.Data);
        LDrawingGroup := TDrawingGroup(LNode.Parent.Data);
        LGroupName := LDrawingGroup.DrawingGroupName;
        if InputQuery(FAppModules.Language.GetString('VNV.EnterDrawingName'),
                      FAppModules.Language.GetString('VNV.DrawingName'),LDrawingName) then
        begin
          LDrawingName := Trim(LDrawingName);
          if (LDrawingName = '') then
            ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameEmpty'))
          else
          begin
            if FDrawingData.DrawingExist(LGroupName,LDrawingName) then
             ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameExists'))
            else
            begin
              LNewDrawing := LDrawingGroup.DrawingList.CreateDrawing(LDrawingName);
              if (LNewDrawing <> nil) then
              begin
                if FDataLoadAgent.CopyDrawing(LDrawingGroup, LNewDrawing, LDrawingGroup, LOldDrawing) then
                begin
                  FCurrentGroupID   := LDrawingGroup.DrawingGroupID;
                  FCurrentDrawingID := LNewDrawing.DrawingID;
                  PopulateTabSheet;
                end
                else
                begin
                  LDrawingGroup.DrawingList.DeleteDrawing(LDrawingName);
                end;
              end;
            end
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserManager.HandleVNVEvent (AVisioApp       : IUnknown;
                                                   AVisioDoc       : IUnknown;
                                                   AVisioEventCode : integer;
                                                   ASourceObj      : IUnknown;
                                                   AEventID        : Integer;
                                                   AEventSeqNum    : Integer;
                                                   ASubjectObj     : IUnknown;
                                                   AMoreInfo       : OleVariant): boolean;
const OPNAME = 'TNetworkVisualiserManager.HandleVNVEvent';
begin
  Result := FALSE;
  try
    if (AVisioEventCode = visEvtCodeDocOpen) then
    begin
      DoBeforeDocumentOpened(Self);
      Result := FVNVEventHandler.HandleVNVEvent(AVisioApp, AVisioDoc, AVisioEventCode,
                                                ASourceObj, AEventID, AEventSeqNum,
                                                ASubjectObj, AMoreInfo);
    end
    else
    if (AVisioEventCode = visEvtDel + visEvtDoc) then
      DoBeforeDocumentClosed(Self)
    else
      Result := FVNVEventHandler.HandleVNVEvent(AVisioApp, AVisioDoc, AVisioEventCode,
                                                ASourceObj, AEventID, AEventSeqNum,
                                                ASubjectObj, AMoreInfo);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserManager.DoBeforeDocumentOpened (ASender : TObject);
const OPNAME = 'TNetworkVisualiserManager.DoBeforeDocumentOpened';
begin
  try
    //FWaitForVisioHandle := CreateEvent(nil, TRUE, FALSE, 'EditingFinished');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserManager.DoBeforeDocumentClosed (ASender : TObject);
const OPNAME = 'TNetworkVisualiserManager.DoBeforeDocumentClosed';
var
  LEventHandle: THandle;
begin
  try
    //if FAppModules.StudyArea.ScenarioLocked then
    //begin
     FAppModules.ScenarioLockManager.ReleaseLock;
     FAppModules.Database.Connected := False;
     FAppModules.Database.Connected := True;

    //end;
    LEventHandle := OpenEvent(EVENT_ALL_ACCESS,True,'EditingFinished');
    if (LEventHandle <> 0) then
      SetEvent(LEventHandle);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserManager.ProcessVNVSpecial (const AParameter: WideString): boolean;
const OPNAME = 'TNetworkVisualiserManager.ProcessVNVSpecial';
begin
  Result := FALSE;
  try
    Result := FVNVEventHandler.ProcessVNVSpecial(AParameter);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserManager.UpdateRegistrySettings : boolean;
const OPNAME = 'TNetworkVisualiserManager.UpdateRegistrySettings';
var
  LRegistry      : TRegistry;
  LSecurityLevel : Integer;
  LRegistryKey   : String;
  LMessage       : string;
  LKeyUpdated    : boolean;
  LSharedMemLocation : string;
begin
  Result := FALSE;
  try
    LRegistry         := TRegistry.Create;
    try
      LRegistry.RootKey := HKEY_LOCAL_MACHINE;
      LRegistryKey      := 'SOFTWARE\Borland\Database Engine\Settings\SYSTEM\INIT';
      if LRegistry.OpenKey(LRegistryKey, False) then
      begin
        LSharedMemLocation := LRegistry.ReadString('SHAREDMEMLOCATION');
        if (LSharedMemLocation = '') then
        begin
          LRegistry.WriteString('SHAREDMEMLOCATION','5BDE');
        end;
        LRegistry.CloseKey;
      end;


      LKeyUpdated := False;
      LRegistry.RootKey := HKEY_CURRENT_USER;
      LRegistryKey      := 'Software\Microsoft\Office\11.0\Visio\Security';
      if LRegistry.OpenKey(LRegistryKey, False) then
      begin
        LSecurityLevel := LRegistry.ReadInteger('Level');
        if LSecurityLevel > 2 then
        begin
          LRegistry.WriteInteger('Level', 2);
        end;
        LRegistry.CloseKey;
        LKeyUpdated := True;
      end;

      LRegistry.RootKey := HKEY_CURRENT_USER;
      LRegistryKey      := 'Software\Microsoft\Office\12.0\Visio\Security';
      if LRegistry.OpenKey(LRegistryKey, False) then
      begin
        LSecurityLevel := LRegistry.ReadInteger('VBAWarnings');
        if LSecurityLevel > 1 then
        begin
          LRegistry.WriteInteger('VBAWarnings', 1);
        end;
        LRegistry.CloseKey;
        LKeyUpdated := True;
      end;

      if not LKeyUpdated then
      begin
        LMessage := FAppModules.Language.GetString('VNV.VisioUpdateMacroSecLevelFailed');
        MessageDlg(LMessage, mtInformation, [mbOK], 0);
      end;

      Result := True;
    finally
      LRegistry.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{ TVisionThread }

procedure TVisioThread.FindVisioWindow;
const OPNAME = 'TVisioThread.FindVisioWindow';
var
  LVisioDialogHandle : HWND;
  LVisioMainHandle   : HWND;

begin
  try
    LVisioMainHandle := GetVisioWindowHandle('Microsoft Visio', True);
    if LVisioMainHandle <> 0 then
    begin
      if LVisioMainHandle = GetForegroundWindow then
      begin
        LVisioDialogHandle := FindWindow(nil, 'Visio');
        if LVisioDialogHandle <> 0 then
          SetForegroundWindow(LVisioDialogHandle);

        LVisioDialogHandle := GetVisioWindowHandle('TVNV', False);
        if LVisioDialogHandle <> 0 then
          SetForegroundWindow(LVisioDialogHandle);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

constructor TVisioThread.Create;
const OPNAME = 'TVisioThread.Create';
begin
  try
    inherited Create(True);
    FreeOnTerminate := True;
    Start; //Resume;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVisioThread.Execute;
const OPNAME = 'TVisioThread.Execute';
begin
  try
    while not Terminated do
    begin
      FindVisioWindow;
      Sleep(100);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TVisioThread.GetVisioWindowHandle(APartOfText: String;AUseTitle: Boolean): HWND;
const OPNAME = 'TVisioThread.GetVisioWindowHandle';
var
  LTempHandle      : HWND;
  LTextLength      : Integer;
  LTempTextChar   : array [0..254] of Char;
  LTempTextString : String;

begin
  Result := 0;
  try
    LTempHandle := FindWindow(nil, nil);
    while (LTempHandle <> 0) do
    begin
      if AUseTitle then
      begin
        LTextLength := GetWindowText(LTempHandle, LTempTextChar, 255);
        LTempTextString :=  LTempTextChar;
        LTempTextString := UpperCase(Copy(LTempTextString, 1, LTextLength));
        APartOfText := UpperCase(APartOfText);
        if Pos(APartOfText, LTempTextString) <> 0 then
          Break;

        LTempHandle := GetWindow(LTempHandle, GW_HWNDNEXT);
      end
      else
      begin
        LTextLength := GetClassName(LTempHandle, LTempTextChar, 255);
        LTempTextString :=  LTempTextChar;
        LTempTextString := UpperCase(Copy(LTempTextString, 1, LTextLength));
        APartOfText := UpperCase(APartOfText);
        if Pos(APartOfText, LTempTextString ) <> 0 then
          Break;

        LTempHandle := GetWindow(LTempHandle, GW_HWNDNEXT);
      end;
    end;
    Result := LTempHandle;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNetworkVisualiserManager.OnVisioThreadTerminate(ASender: TObject);
const OPNAME = 'TNetworkVisualiserManager.OnVisioThreadTerminate';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNetworkVisualiserManager.UpdateCOMIniFile: boolean;
const OPNAME = 'TNetworkVisualiserManager.UpdateCOMIniFile';
var
  LINIFile  : TIniFile;
 //LBinStr   : string;
  LFilename : string;
  lMsg      : string;
  FCurrentSource : integer;
begin
  Result := False;
  try
    //LFilename := ApplicationExeName;
    //LFilename := ExtractFilePath(LFilename);
    LFilename := GetAppDataLocalDir;
    {
    if (Length(LFilename) > 4) then
    begin
      LBinStr := Copy(LFilename,Length(LFilename)- 3,4);
      if (UpperCase(LBinStr) = 'BIN\') then
         LFilename := Copy(LFilename,1,Length(LFilename)- 4);
    end;
    }
    LFilename := IncludeTrailingPathDelimiter(LFilename);
    LFilename := LFilename + 'VoaimsCom.ini' ;
    if not FileExists(LFilename) then
    begin
      lMsg := Format(FAppModules.Language.GetString('VNV.FailedToUpdateIniFile'), [LFilename]);;
      raise Exception.Create(lMsg);
    end
    else
    begin
      LINIFile := TIniFile.Create(LFilename);
      try

        LINIFile.WriteString('STUDY','Study',FAppModules.StudyArea.StudyLabel);
        LINIFile.WriteString('STUDY','Model',FAppModules.StudyArea.ModelLabel);
        LINIFile.WriteString('STUDY','SubArea',FAppModules.StudyArea.SubAreaLabel);
        LINIFile.WriteString('STUDY','Scenario',FAppModules.StudyArea.ScenarioLabel);
        LINIFile.WriteString('STUDY','AutoStudy','N');
        LINIFile.WriteString('USER', 'UserID',   FAppModules.User.UserId);
        LINIFile.WriteString('USER', 'Password', FAppModules.User.Password);
        LINIFile.WriteString('USER', 'Language', FAppModules.User.PreferedLanguage);
        LINIFile.WriteString('USER','AutoLogon','N');
        FCurrentSource := Ord(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.DataSources.CurrentSource);
        LINIFile.WriteInteger('Settings','SummaryOutputDataSourceName',Ord(FCurrentSource));
        LINIFile.UpdateFile;
        Result := True;
      finally
        FreeAndNil(LINIFile);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;end;

end.

