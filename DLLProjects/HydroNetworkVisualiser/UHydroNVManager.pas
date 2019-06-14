{******************************************************************************}
{* UNIT : Contains the class UHydroNVManager.                                 *}
{*        (Network Visualiser for Hydrology model)                            *}
{******************************************************************************}

unit UHydroNVManager;

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
  UAbstractModelObjects,
  UHydroNVSheet,
  Registry,
  UPageViewer,
  Messages,
  VCL.ExtCtrls,
  UHydroNVEventHandler,
  HydrologyCom_TLB,
  UHydroNVDrawing;

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
  THydroNVManager = class(TVisioTabSheetManager)
  private
  protected
    FVisioThread               : TVisioThread;
    FHydroNVEventHandler       : THydroNVEventHandler;
    FContextMenu               : TPopupMenu;
    FToggleDrawingReadOnlyFlag : TMenuItem;
    FCopyDrawingMenuItem       : TMenuItem;
    FPasteDrawingMenuItem      : TMenuItem;
    FCurrentDrawingID          : integer;
    FPageViewer                : TPageViewer;
    FSystemFlag                : boolean;
    FCopiedDrawing             : IHydroNVDrawing;
    //FWaitForVisioHandle        : THandle;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure PopulateTabSheet;
    procedure DoTreeNodeHasChanged(ASender: TObject; ANode: TTreeNode);
    procedure OnNewDrawing;
    procedure OnDeleteDrawing;
    procedure OnRenameDrawing;
    procedure OnEditDrawing;
    procedure OnViewDrawing;
    procedure OnCopyDrawing;
    procedure OnNameEdited (Sender: TObject; Node: TTreeNode; var AName : string);
    function UpdateDrawingName(ANode: TTreeNode; var ANewName: string): boolean;
    function SetDrawingReadOnlyFlag(ANode: TTreeNode): boolean;
    function RemoveDrawingReadOnlyFlag(ANode: TTreeNode): boolean;
    procedure OnTreeViewDblClick(Sender: TObject);
    function HydroNVSheet: THydroNVSheet;

    procedure OnToggleDrawingReadOnlyFlagClick(Sender: TObject);
    procedure OnCopyDrawingClick(Sender: TObject);
    procedure OnPasteDrawingClick(Sender: TObject);
    procedure OnPopupEvent(Sender: TObject);
    function InOCXMode:boolean;
    procedure DoBeforeDocumentClosed (ASender : TObject);
    procedure DoBeforeDocumentOpened (ASender : TObject);

    procedure OnVisioThreadTerminate(ASender: TObject);
    function GetVSDFileName (ADrawingName : string) : string;
    function UpdateCOMIniFile: boolean;
  public
    function Initialise: Boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged (AContext : TChangeContext;
                                  AFieldName,AOldValue,ANewValue: string): boolean; override;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean; override;
    function SetVisioMacroSecurityLevel : boolean;
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
  UAbstractComponent,
  UErrorHandlingOperations, UTabSheetManager;

function THydroNVManager.InOCXMode: boolean;
const OPNAME = 'THydroNVManager.InOCXMode';
begin
  Result := False;
  try
    Result := not Assigned(FAppModules.MainForm());
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVManager.CreateMemberObjects;
const OPNAME = 'THydroNVManager.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FTabSheet         := nil;
    FCurrentDrawingID := -1;
    FSystemFlag       := FALSE;
    FCopiedDrawing    := nil;
    if InOCXMode then
    begin
      FHydroNVEventHandler     := THydroNVEventHandler.Create(FAppModules);
      FVisioThread             := TVisioThread.Create;
      FVisioThread.OnTerminate := OnVisioThreadTerminate;
    end
    else
    begin
      FTabSheet           := THydroNVSheet.Create(nil, FAppModules);
      FPageViewer         := TPageViewer.Create(FTabSheet,FAppModules);
      FPageViewer.Parent  := FTabSheet;

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

      HydroNVSheet.TreeView.PopupMenu := FContextMenu;

      HydroNVSheet.TreeView.OnChange   := DoTreeNodeHasChanged;
      HydroNVSheet.TreeView.OnEdited   := OnNameEdited;
      HydroNVSheet.TreeView.OnDblClick := OnTreeViewDblClick;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVManager.DestroyMemberObjects;
const OPNAME = 'THydroNVManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    if InOCXMode then
    begin
      FreeAndNil(FHydroNVEventHandler);
      FVisioThread.Terminate;
    end
    else
    begin
      FreeAndNil(FContextMenu);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVManager.HydroNVSheet: THydroNVSheet;
const OPNAME = 'THydroNVManager.HydroNVSheet';
begin
  Result := nil;
  try
    if Assigned(FTabSheet)  then
      Result := THydroNVSheet(FTabSheet);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVManager.Initialise: Boolean;
const OPNAME = 'THydroNVManager.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVManager.LanguageHasChanged: boolean;
const OPNAME = 'THydroNVManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVManager.PopulateTabSheet;
const OPNAME = 'THydroNVManager.PopulateTabSheet';
var
  LDrawingIndex      : integer;
  LDrawingAgent      : IHydroNVDrawingAgent;
  LDrawing           : IHydroNVDrawing;
  LDrawingNode       : TTreeNode;
  LTree              : TTreeView;
  LSelectedNode      : TTreeNode;
  LHydrologyModel    : IHydrologyModel;
begin
  try
    if InOCXMode then Exit;
    LTree := HydroNVSheet.TreeView;
    FSystemFlag := TRUE;
    LTree.Items.Clear;
    LSelectedNode := nil;
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
    for LDrawingIndex := 0 to LDrawingAgent.HydroNVDrawingCount -1 do
    begin
      LDrawing := LDrawingAgent.HydroNVDrawingByIndex[LDrawingIndex];
      LDrawingNode := LTree.Items.AddObjectFirst(nil, LDrawing.DrawingName, Pointer(LDrawing.DrawingID));
      if (LDrawing.DrawingID = FCurrentDrawingID) then
        LSelectedNode := LDrawingNode;
    end;
    LTree.AlphaSort(False);
    LTree.ReadOnly := False;
    if (LSelectedNode = nil) AND (LTree.Items.Count > 0) then
      LSelectedNode := LTree.Items[0];
    FSystemFlag := FALSE;
    LTree.Selected := LSelectedNode;
    if (LTree.Selected = nil) then
      LTree.OnChange(Self, nil);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVManager.StudyHasChanged: boolean;
const OPNAME = 'THydroNVManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if not InOCXMode then
    begin
      PopulateTabSheet;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVManager.StudyDataHasChanged (AContext   : TChangeContext;
                                                        AFieldName : string;
                                                        AOldValue  : string;
                                                        ANewValue  : string): boolean;
const OPNAME = 'THydroNVManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Assigned(FHydroNVEventHandler) then
      Result := Result and FHydroNVEventHandler.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVManager.GetVSDFileName (ADrawingName : string) : string;
const OPNAME = 'THydroNVManager.GetVSDFileName';
var
  lFileName : string;
  lPath     : string;
begin
  Result := '';
  try
    LPath := NetworkDiagramsPath +
             ChopCharacters(FAppModules.StudyArea.StudyAreaCode) + '\' +
             ChopCharacters(FAppModules.StudyArea.ScenarioCode)  + '\';
    if not DirectoryExists(LPath) then
       ForceDirectories(LPath);
    LFileName := LPath + ADrawingName + '.VSD';
    Result := LFileName;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure THydroNVManager.DoTreeNodeHasChanged(ASender: TObject;ANode: TTreeNode);
const OPNAME = 'THydroNVManager.DoTreeNodeHasChanged';
Var
  LFileName       : string;
begin
  try
    FPageViewer.Visible := False;
    if (NOT FSystemFlag) then
    begin
      FPageViewer.ClearPages;
      if (ANode = nil) then
      begin
        HydroNVSheet.MenuItemManager.SetMenuNewDrawing(msEnable);
        HydroNVSheet.MenuItemManager.SetMenuDeleteDrawing(msDisable);
        HydroNVSheet.MenuItemManager.SetMenuRenameDrawing(msDisable);
        HydroNVSheet.MenuItemManager.SetMenuViewDrawing(msDisable);
        HydroNVSheet.MenuItemManager.SetMenuEditDrawing(msDisable);
        HydroNVSheet.MenuItemManager.SetMenuCopyDrawing(msDisable);
        FCurrentDrawingID := -1;
      end
      else
      if ANode.Level = 0 then
      begin
        HydroNVSheet.MenuItemManager.SetMenuNewDrawing(msEnable);
        LFileName := GetVSDFileName(ANode.Text);
        if (NOT FileExists(LFileName)) then
        begin
          HydroNVSheet.MenuItemManager.SetMenuDeleteDrawing(msDisable);
          HydroNVSheet.MenuItemManager.SetMenuRenameDrawing(msDisable);
          HydroNVSheet.MenuItemManager.SetMenuViewDrawing(msDisable);
          HydroNVSheet.MenuItemManager.SetMenuEditDrawing(msDisable);
          HydroNVSheet.MenuItemManager.SetMenuCopyDrawing(msDisable);
          HydroNVSheet.HintDisplay.Panels[1].Text := 'File (' + LFileName + ') does not exist.';
        end
        else
        begin
          HydroNVSheet.MenuItemManager.SetMenuDeleteDrawing(msEnable);
          HydroNVSheet.MenuItemManager.SetMenuRenameDrawing(msEnable);
          HydroNVSheet.MenuItemManager.SetMenuViewDrawing(msEnable);
          HydroNVSheet.MenuItemManager.SetMenuCopyDrawing(msEnable);
          HydroNVSheet.MenuItemManager.SetMenuEditDrawing(msEnable);
          FCurrentDrawingID := Integer(ANode.Data);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;
{$WARN SYMBOL_PLATFORM ON}

function THydroNVManager.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): boolean;
const OPNAME = 'THydroNVManager.DoCustomTabSheetEvent';
var
  lMsg       : string;
begin
  Result := inherited DoCustomTabSheetEvent(ACustomModelEvent);
  try
    if not Result then
    begin
      Result := True;
      case ACustomModelEvent.Action of
        meHydroNVNewDrawing    : OnNewDrawing;
        meHydroNVViewDrawing   : OnViewDrawing;
        meHydroNVCopyDrawing   : OnCopyDrawing;
        meHydroNVDeleteDrawing :
        	begin
            lMsg := FAppModules.Language.GetString('VNV.ConfirmDeleteDrawing');
            if MessageDlg(lMsg, mtConfirmation, [mbYes,mbCancel], 0) = mrCancel then
              exit;
            OnDeleteDrawing;
          end;
        meHydroNVRenameDrawing : OnRenameDrawing;
        meHydroNVEditDrawing   : OnEditDrawing;
        else
          Result := False;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVManager.UpdateDrawingName(ANode: TTreeNode; var ANewName: string): boolean;
const OPNAME = 'THydroNVManager.UpdateDrawingName';
var
  LDrawingID      : integer;
  LDrawing        : IHydroNVDrawing;
  LOldFileName    : string;
  LNewFileName    : string;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LOldName        : String;
begin
  Result := False;
  try
    if (ANode.Level = 0) then
    begin
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
      if LDrawingAgent.DrawingExists[ANewName] then
        ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameExists'))
      else
      begin
        LDrawingID := Integer(ANode.Data);
        LDrawing   := LDrawingAgent.HydroNVDrawingByID[LDrawingID];
        LOldName   := LDrawing.DrawingName;
        LDrawing.DrawingName := ANewName;
        if (LDrawing.DrawingName = ANewName) then
        begin
          LOldFileName := GetVSDFileName(LOldName);
          LNewFileName := GetVSDFileName(ANewName);
          RenameFile(lOldFileName, lNewFileName);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVManager.OnRenameDrawing;
const OPNAME = 'THydroNVManager.OnRenameDrawing';
var
  LNode           : TTreeNode;
  LDrawingID      : integer;
  LDrawing        : IHydroNVDrawing;
  LOldFileName    : string;
  LNewFileName    : string;
  LOldName        : String;
  LNewName        : string;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
begin
  try
    FPageViewer.Visible := False;
    LNode := HydroNVSheet.TreeView.Selected;
    if (LNode = nil) then
      ShowMessage(FAppModules.Language.GetString('VNV.SelectDrawing'))
    else
    begin
      if (LNode.Data <> nil) then
      begin
        if InputQuery(FAppModules.Language.GetString('VNV.EnterNewDrawingName'),
                      FAppModules.Language.GetString('VNV.NewDrawingName'), lNewName) then
        begin
          LNewName := Trim(lNewName);
          if (LNewName = '') then
            ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameEmpty'))
          else
          begin
            LHydrologyModel := FAppModules.Model as IHydrologyModel;
            LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;

            if (LDrawingAgent.DrawingExists[LNewName]) then
              ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameExists'))
            else
            begin
              LDrawingID := Integer(LNode.Data);
              LDrawing   := LDrawingAgent.HydroNVDrawingByID[LDrawingID];
              LOldName   := LDrawing.DrawingName;
              LDrawing.DrawingName := lNewName;
              if (LDrawing.DrawingName = LNewName) then
              begin
                LOldFileName := GetVSDFileName(LOldName);
                LNewFileName := GetVSDFileName(LNewName);
                RenameFile(lOldFileName, lNewFileName);
                LNode.Text := LNewName;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVManager.OnNewDrawing;
const OPNAME = 'THydroNVManager.OnNewDrawing';
var
  LDrawingName    : string;
  LDrawing        : IHydroNVDrawing;
  LGISDrawing     : Integer;
  LMesgDlgResult  : Word;
  LMsg            : string;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LNetworkID      : Integer;
begin
  try
    FPageViewer.Visible := False;
    LMsg := FAppModules.Language.GetString('VNV.DrawingGISConfirmation');
    LMesgDlgResult := WRMFMessageDialog(LMsg,mtConfirmation,mbYesNoCancel,
                                         [FAppModules.Language.GetString('VNV.DrawingGIS'),
                                         FAppModules.Language.GetString('VNV.DrawingNoGIS')]);
    if (LMesgDlgResult = mrCancel) then Exit;
    LGISDrawing := 0;
    if (LMesgDlgResult = mrYes) then
      LGISDrawing := 1;

    if InputQuery(FAppModules.Language.GetString('VNV.EnterDrawingName'),
                  FAppModules.Language.GetString('VNV.DrawingName'),LDrawingName) then
    begin
      LDrawingName := Trim(LDrawingName);
      if (LDrawingName = '') then
        ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameEmpty'))
      else
      begin
        LHydrologyModel := FAppModules.Model as IHydrologyModel;
        LNetworkID      := LHydrologyModel.Network.NetworkID;
        LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
        if (LDrawingAgent.DrawingExists[LDrawingName]) then
          ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameExists'))
        else
        begin
          LDrawingName := GetVSDFileName(LDrawingName);
          LDrawing := LDrawingAgent.CreateNewDrawing(LNetworkID, LDrawingName, LGISDrawing, '');
          if (LDrawing <> nil) then
          begin
            PopulateTabSheet;
          end;
        end
      end
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVManager.OnCopyDrawing;
const OPNAME = 'THydroNVManager.OnCopyDrawing';
var
  LNewDrawingName : string;
  LOldDrawingName : string;
  LNode           : TTreeNode;
  LOldDrawing     : IHydroNVDrawing;
  LNewDrawing     : IHydroNVDrawing;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LNetworkID      : Integer;
  LDrawingID      : Integer;
begin
  try
    FPageViewer.Visible := False;
    LNode := HydroNVSheet.TreeView.Selected;
    if (LNode.Level <> 0) then
       ShowMessage(FAppModules.Language.GetString('VNV.SelectDrawing'))
    else
    begin
      if (LNode.Data <> nil) then
      begin
        LHydrologyModel := FAppModules.Model as IHydrologyModel;
        LNetworkID      := LHydrologyModel.Network.NetworkID;
        LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
        LDrawingID      := Integer(LNode.Data);
        LOldDrawing     := LDrawingAgent.HydroNVDrawingByID[LDrawingID];
        if InputQuery(FAppModules.Language.GetString('VNV.EnterDrawingName'),
                      FAppModules.Language.GetString('VNV.DrawingName'), LNewDrawingName) then
        begin
          LNewDrawingName := Trim(LNewDrawingName);
          if (LNewDrawingName = '') then
            ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameEmpty'))
          else
          begin
            if (LDrawingAgent.DrawingExists[LNewDrawingName]) then
             ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameExists'))
            else
            begin
              LNewDrawingName := GetVSDFileName(LNewDrawingName);
              LOldDrawingName := GetVSDFileName(LOldDrawing.DrawingName);
              LNewDrawing     := LDrawingAgent.CreateNewDrawing(LNetworkID, LNewDrawingName, LOldDrawing.GISDrawing, LOldDrawingName);

              if (LNewDrawing <> nil) then
              begin
                PopulateTabSheet;
              end;
            end
          end
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVManager.OnDeleteDrawing;
const OPNAME = 'THydroNVManager.OnDeleteDrawing';
var
  LNode           : TTreeNode;
  LDrawingID      : Integer;
  LDrawingName    : string;
  LIndex          : integer;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LDrawing        : IHydroNVDrawing;
  LFileName       : String;
begin
  try
    FPageViewer.Visible := False;
    LNode := HydroNVSheet.TreeView.Selected;
    if (LNode = nil) then
      ShowMessage(FAppModules.Language.GetString('VNV.SelectDrawing'))
    else
    begin
      if (LNode.Data <> nil) then
      begin
        LIndex          := LNode.Index;
        LDrawingID      := Integer(LNode.Data);
        LDrawingName    := LNode.Text;
        LHydrologyModel := FAppModules.Model as IHydrologyModel;
        LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
        LFileName       := GetVSDFileName(LDrawingName);
        if (LDrawingAgent.DeleteHydroNVDrawing(LDrawingID, LFilename)) then
        begin
          if (LDrawingAgent.HydroNVDrawingCount = 0) then
            FCurrentDrawingID := -1
          else
          begin
            if (LIndex >= 0) AND (LIndex < LDrawingAgent.HydroNVDrawingCount) then
            begin
              LDrawing          := LDrawingAgent.HydroNVDrawingByIndex[lIndex];
              FCurrentDrawingID := LDrawing.DrawingID;
            end;
          end;
          PopulateTabSheet;
        end;
        FPageViewer.ClearPages;
       end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure THydroNVManager.OnTreeViewDblClick(Sender: TObject);
const OPNAME = 'THydroNVManager.OnTreeViewDblClick';
begin
	try
    OnViewDrawing
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVManager.OnViewDrawing;
const OPNAME = 'THydroNVManager.OnViewDrawing';
var
  LDrawingName    : string;
  LDrawingID      : integer;
  LFileName       : string;
  LNode           : TTreeNode;
  LDrawingApp     : TDrawingControl;
  LIndex          : integer;
  LTempStr        : string;
  LImageViewer    : TImageViewer;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LDrawing        : IHydroNVDrawing;
begin
  try
    FPageViewer.Visible := False;
  	LNode := HydroNVSheet.TreeView.Selected;
    case LNode.Level of
      0:
      begin
        HydroNVSheet.MenuItemManager.SetMenuNewDrawing(msEnable);
        HydroNVSheet.MenuItemManager.SetMenuDeleteDrawing(msEnable);
        LDrawingName  := LNode.Text;
        LDrawingID    := Integer(LNode.Data);
        LHydrologyModel := FAppModules.Model as IHydrologyModel;
        LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
        LDrawing        := LDrawingAgent.HydroNVDrawingByID[LDrawingID];
        if (LDrawing.ReadOnly = 1) then
        	HydroNVSheet.MenuItemManager.SetMenuEditDrawing(msDisable)
        else
	        HydroNVSheet.MenuItemManager.SetMenuEditDrawing(msEnable);
        LFileName := GetVSDFileName(LDrawingName);
        if FileExists(LFileName) then
        begin
          FPageViewer.Visible := True;

          // Generate the JPEG files from the VSD file
          LDrawingApp := TDrawingControl.Create(nil);
          try
            LDrawingApp.Visible := false;
            LDrawingApp.Parent  := HydroNVSheet;
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

procedure THydroNVManager.OnEditDrawing;
const OPNAME = 'THydroNVManager.OnEditDrawing';
var
  LExecInfo       : SHELLEXECUTEINFOA;
	lFileName       : string;
  LNode           : TTreeNode;
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
    LNode := HydroNVSheet.TreeView.Selected;
    if LNode = nil then
  	  exit;
    FPageViewer.ClearPages;
    if (LNode.Level = 0) then
    begin
      lDrawingName  := LNode.Text;
      Application.ProcessMessages;
      lFileName := GetVSDFileName(lDrawingName);
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

//        if UpdateCOMIniFile(FAppModules) then
        if (UpdateCOMIniFile) then
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
            SetVisioMacroSecurityLevel;
            ZeroMemory(@LExecInfo,sizeof(LExecInfo));
            LExecInfo.cbSize := sizeof(LExecInfo);
            //LExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
            LExecInfo.lpVerb := 'open';
            LExecInfo.lpFile := PAnsiChar(AnsiString(lFileName));
            LExecInfo.nShow := SW_SHOWMAXIMIZED;
            if (ShellExecuteEx(@LExecInfo) = false)then
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
          finally
            if not TAbstractMainForm(FAppModules.MainForm.MainForm).Closing then
            begin
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

function THydroNVManager.UpdateCOMIniFile: boolean;
const OPNAME = 'THydroNVManager.UpdateCOMIniFile';
var
  LINIFile  : TIniFile;
  LBinStr   : string;
  LFilename : string;
  lMsg      : string;
//  FCurrentSource : integer;
begin
  Result := False;
  try
    LFilename := ApplicationExeName;
    LFilename := ExtractFilePath(LFilename);
    if (Length(LFilename) > 4) then
    begin
      LBinStr := Copy(LFilename,Length(LFilename)- 3,4);
      if (UpperCase(LBinStr) = 'BIN\') then
         LFilename := Copy(LFilename,1,Length(LFilename)- 4);
    end;
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
//        FCurrentSource := Ord(TYieldModelDataObject(FAppModules.Model.ModelData).OutputData.CastSummaryOutputData.DataSources.CurrentSource);
//        LINIFile.WriteInteger('Settings','SummaryOutputDataSourceName',Ord(FCurrentSource));
        Result := True;
      finally
        FreeAndNil(LINIFile);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVManager.OnNameEdited (Sender: TObject; Node: TTreeNode; var AName : string);
const OPNAME = 'THydroNVManager.OnNameEdited';
begin
  try
    case Node.Level of
      0: UpdateDrawingName(Node, AName)
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVManager.SetDrawingReadOnlyFlag(ANode: TTreeNode): boolean;
const OPNAME = 'THydroNVManager.SetDrawingReadOnlyFlag';
var
  LDrawingID      : integer;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LDrawing        : IHydroNVDrawing;
begin
  Result := False;
  try
    if (ANode.Level = 0) then
    begin
      LDrawingID      := Integer(ANode.Data);
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
      LDrawing        := LDrawingAgent.HydroNVDrawingByID[LDrawingID];
      LDrawing.ReadOnly := 1;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVManager.RemoveDrawingReadOnlyFlag(ANode: TTreeNode): boolean;
const OPNAME = 'THydroNVManager.RemoveDrawingReadOnlyFlag';
var
  LDrawingID      : integer;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LDrawing        : IHydroNVDrawing;
begin
  Result := False;
  try
    if (ANode.Level = 0) then
    begin
      LDrawingID      := Integer(ANode.Data);
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
      LDrawing        := LDrawingAgent.HydroNVDrawingByID[LDrawingID];
      LDrawing.ReadOnly := 0;
      Result := TRUE;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVManager.OnToggleDrawingReadOnlyFlagClick(Sender: TObject);
const OPNAME = 'THydroNVManager.OnToggleDrawingReadOnlyFlagClick';
var
 	LNode           : TTreeNode;
  LDrawingID      : integer;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LDrawing        : IHydroNVDrawing;
begin
	try
    LNode := HydroNVSheet.TreeView.Selected;
    if LNode = nil then // No selection
      exit;
    if LNode.Level <> 0 then
      exit;
    LDrawingID      := Integer(LNode.Data);
    LHydrologyModel := FAppModules.Model as IHydrologyModel;
    LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
    LDrawing        := LDrawingAgent.HydroNVDrawingByID[LDrawingID];
    if not FToggleDrawingReadOnlyFlag.Checked then
    begin
      SetDrawingReadOnlyFlag(LNode);
      HydroNVSheet.MenuItemManager.SetMenuEditDrawing(msDisable);
    end
    else
    begin
      RemoveDrawingReadOnlyFlag(LNode);
      HydroNVSheet.MenuItemManager.SetMenuEditDrawing(msEnable);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVManager.OnCopyDrawingClick(Sender: TObject);
const OPNAME = 'THydroNVManager.OnCopyDrawingClick';
var
  LNode           : TTreeNode;
  LDrawingID      : Integer;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
begin
  try
    LNode := HydroNVSheet.TreeView.Selected;
    if (LNode <> nil) AND (LNode.Level = 0) then
    begin
      LDrawingID      := Integer(LNode.Data);
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
      FCopiedDrawing  := LDrawingAgent.HydroNVDrawingByID[LDrawingID];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVManager.OnPasteDrawingClick(Sender: TObject);
const OPNAME = 'THydroNVManager.OnPasteDrawingClick';
var
  LNetworkID      : Integer;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LNewDrawing     : IHydroNVDrawing;
  LOldDrawingName : String;
  LNewDrawingName : String;
begin
	try
    if (FCopiedDrawing <> nil) then
    begin
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LNetworkID      := LHydrologyModel.Network.NetworkID;
      LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
      if InputQuery(FAppModules.Language.GetString('VNV.NetworkVisualiser'),
                    FAppModules.Language.GetString('VNV.EnterNewDrawingName'), LNewDrawingName) then
      begin
        LNewDrawingName := Trim(LNewDrawingName);
        if (LNewDrawingName = '') then
          ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameEmpty'))
        else
        begin
          if (LDrawingAgent.DrawingExists[LNewDrawingName]) then
           ShowMessage(FAppModules.Language.GetString('VNV.DrawingNameExists'))
          else
          begin
            LNewDrawingName := GetVSDFileName(LNewDrawingName);
            LOldDrawingName := GetVSDFileName(FCopiedDrawing.DrawingName);
            LNewDrawing     := LDrawingAgent.CreateNewDrawing(LNetworkID, LNewDrawingName, FCopiedDrawing.GISDrawing, LOldDrawingName);

            if (LNewDrawing <> nil) then
            begin
              PopulateTabSheet;
            end;
          end
        end
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVManager.OnPopupEvent(Sender: TObject);
const OPNAME = 'THydroNVManager.OnPopupEvent';
var
  LNode           : TTreeNode;
  lIndex          : integer;
  LDrawingID      : integer;
  LHydrologyModel : IHydrologyModel;
  LDrawingAgent   : IHydroNVDrawingAgent;
  LDrawing        : IHydroNVDrawing;
begin
	try
    LNode := HydroNVSheet.TreeView.Selected;
    if LNode = nil then
    begin
      for lIndex := 0 to FContextMenu.Items.Count - 1  do
        FContextMenu.Items.Items[lIndex].Enabled := false;
      exit;
    end;
    if (LNode.Level = 0) then
    begin
      FToggleDrawingReadOnlyFlag.Enabled := TRUE;
      FCopyDrawingMenuItem.Enabled := TRUE;
      FPasteDrawingMenuItem.Enabled := FALSE;
      LDrawingID      := Integer(LNode.Data);
      LHydrologyModel := FAppModules.Model as IHydrologyModel;
      LDrawingAgent   := LHydrologyModel.Network.HydroNVDrawingAgent;
      LDrawing        := LDrawingAgent.HydroNVDrawingByID[LDrawingID];
      if (LDrawing.ReadOnly = 1) then
        FToggleDrawingReadOnlyFlag.Checked := TRUE
      else
        FToggleDrawingReadOnlyFlag.Checked := FALSE;
      if (FCopiedDrawing <> nil) then
        FPasteDrawingMenuItem.Enabled := TRUE
      else
        FPasteDrawingMenuItem.Enabled := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVManager.HandleVNVEvent (AVisioApp       : IUnknown;
                                                   AVisioDoc       : IUnknown;
                                                   AVisioEventCode : integer;
                                                   ASourceObj      : IUnknown;
                                                   AEventID        : Integer;
                                                   AEventSeqNum    : Integer;
                                                   ASubjectObj     : IUnknown;
                                                   AMoreInfo       : OleVariant): boolean;
const OPNAME = 'THydroNVManager.HandleVNVEvent';
begin
  Result := FALSE;
  try
    if (AVisioEventCode = visEvtCodeDocOpen) then
    begin
      DoBeforeDocumentOpened(Self);
      Result := FHydroNVEventHandler.HandleVNVEvent(AVisioApp, AVisioDoc, AVisioEventCode,
                                                ASourceObj, AEventID, AEventSeqNum,
                                                ASubjectObj, AMoreInfo);
    end
    else
    if (AVisioEventCode = visEvtDel + visEvtDoc) then
      DoBeforeDocumentClosed(Self)
    else
      Result := FHydroNVEventHandler.HandleVNVEvent(AVisioApp, AVisioDoc, AVisioEventCode,
                                                ASourceObj, AEventID, AEventSeqNum,
                                                ASubjectObj, AMoreInfo);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVManager.DoBeforeDocumentOpened (ASender : TObject);
const OPNAME = 'THydroNVManager.DoBeforeDocumentOpened';
begin
  try
    //FWaitForVisioHandle := CreateEvent(nil, TRUE, FALSE, 'EditingFinished');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroNVManager.DoBeforeDocumentClosed (ASender : TObject);
const OPNAME = 'THydroNVManager.DoBeforeDocumentClosed';
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

function THydroNVManager.ProcessVNVSpecial (const AParameter: WideString): boolean;
const OPNAME = 'THydroNVManager.ProcessVNVSpecial';
begin
  Result := FALSE;
  try
    Result := FHydroNVEventHandler.ProcessVNVSpecial(AParameter);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroNVManager.SetVisioMacroSecurityLevel : boolean;
const OPNAME = 'THydroNVManager.SetVisioMacroSecurityLevel';
var
  LRegistry      :   TRegistry;
  LSecurityLevel :   Integer;
  LRegistryKey   :   String;
  LVersion       :   string;
  LMessage       :   string;
begin
  Result := FALSE;
  try
    LRegistry         := TRegistry.Create;
    try
      LVersion := '11.0';
      LRegistry.RootKey := HKEY_CLASSES_ROOT;
      LRegistryKey  := 'Visio.Application\CurVer';
      if LRegistry.OpenKey(LRegistryKey, False) then
      begin
        LVersion := LRegistry.ReadString('');
        LVersion := Copy(LVersion,19,Length(LVersion));
        LRegistry.CloseKey;
        if(Pos('.',LVersion) = 0) then
          LVersion := LVersion + '.0';
      end;

      LRegistry.RootKey := HKEY_CURRENT_USER;
      LRegistryKey      := 'Software\Microsoft\Office\'+ LVersion+ '\Visio\Security';
      if LRegistry.OpenKey(LRegistryKey, False) then
      begin
        LSecurityLevel := LRegistry.ReadInteger('Level');
        if LSecurityLevel > 2 then
        begin
          LRegistry.WriteInteger('Level', 2);
        end;
        LRegistry.CloseKey;
        Result := True;
      end
      else
      begin
        LMessage := FAppModules.Language.GetString('VNV.VisioUpdateMacroSecLevelFailed');
        MessageDlg(LMessage, mtInformation, [mbOK], 0);
      end;
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
    inherited Create( True );
    FreeOnTerminate := True;
    //Resume;
    Start;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TVisioThread.Execute;
const OPNAME = 'TVisioThread.Execute';
begin
  try
    while not Terminated do
    begin
      FindVisioWindow;
      Sleep( 100 );
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

procedure THydroNVManager.OnVisioThreadTerminate(ASender: TObject);
const OPNAME = 'THydroNVManager.OnVisioThreadTerminate';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

