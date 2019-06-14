//
//
//  UNIT      : Contains TIFRModelManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit UIFRModelManager;

interface

uses

  Contnrs,
  Classes,
  VCL.ComCtrls,
  UAbstractObject,
  UGenericModelLinkClasses,
  USystemModelManager,
  UIFRDataObject,
  UIFRTabSheetManager,
  UIFRGUIManager,
  UIFRMenuItemManager;

type

  TIFRModelManager = class(TSystemModelManager)
  protected
    FModelData                             : TIFRModelData;
    FMenuItemManager                       : IFRMenuItemManager;
    FIFRTabSheetManager                    : TIFRTabSheetManager;
    FModelGUIManager                       : TIFRGUIManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function AddAndSelectElement(AViewID : string; AParentID : string; AWeighting : integer; ACaption : string;
             ABitmapName : string; ADataType: string; ASelect : boolean = True): boolean;
    function DeleteElement(ATreeNode : TTreeNode) : boolean;
    procedure DoLaunchIFRUserGuide;
    function LoadModelData: boolean;
  public
    function DoCreateIFRSite(ACSVFileName: string): TIFRSiteDataObject;
    function DoDeleteIFRSite(AIFRSiteID: integer): boolean;
    function DoSaveIFRSite(AIFRSiteID: integer): boolean;

    function ModelName: string; override;
    function Initialise: boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function ViewInputDialog(AParent: TObject; ACommaTextContextData: String;AOwner: TObject = nil): Boolean; override;
    function LanguageHasChanged: Boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    function StudyHasChanged: Boolean; override;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; override;
    function ProcessCustomModelEvent(AData: TModelMenuData): boolean; override;
    function ModelData: TInterfacedObject; override;
  end;

implementation

uses
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  UConstants,
  UViewDataItem,
  UAbstractComponent,
  UTreeViewTabSheet,
  UMainMenuEventType,
  UIFRDataLoadAgent,
  UIsAcrobatReaderInstalled,
  UPDFDocumentLauncher,
  UErrorHandlingOperations, Math;

procedure TIFRModelManager.CreateMemberObjects;
const OPNAME = 'TIFRModelManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FModelData := TIFRModelData.Create(FAppModules);
    if Assigned(FAppModules.MainForm()) and Assigned(FAppModules.MainForm.PageControl) then
    begin
      FMenuItemManager := IFRMenuItemManager.Create(FAppModules);
      FIFRTabSheetManager := TIFRTabSheetManager.Create(FAppModules);
      FIFRTabSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
      FModelGUIManager := TIFRGUIManager.Create(FAppModules);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRModelManager.DestroyMemberObjects;
const OPNAME = 'TIFRModelManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    if Assigned(FIFRTabSheetManager) then
      FreeAndNil(FIFRTabSheetManager);
    if Assigned(FMenuItemManager) then
      FreeAndNil(FMenuItemManager);
    if Assigned(FModelGUIManager) then
      FreeAndNil(FModelGUIManager);
    if Assigned(FModelData) then
      FreeAndNil(FModelData);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelManager.Initialise: boolean;
const OPNAME = 'TIFRModelManager.Initialise';
begin
  Result := inherited Initialise;
  try
    if Assigned(FModelData) then
      FModelData.Initialise;
    if Assigned(FIFRTabSheetManager) then
      Result := Result and FIFRTabSheetManager.Initialise;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelManager.LanguageHasChanged: Boolean;
const OPNAME = 'TIFRModelManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned (FIFRTabSheetManager) then
      Result := Result and FIFRTabSheetManager.LanguageHasChanged;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelManager.ModelData: TInterfacedObject;
const OPNAME = 'TIFRModelManager.ModelData';
begin
  Result := nil;
  try
    Result := FModelData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRModelManager.ModelName: string;
const OPNAME = 'TIFRModelManager.ModelName';
begin
  Result := '';
  try
    Result := CIFRPreProcessor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRModelManager.ProcessCustomModelEvent (AData : TModelMenuData) : boolean;
const OPNAME = 'TIFRModelManager.ProcessCustomModelEvent';
begin
  Result := False;
  try
    if Assigned(FAppModules.MainForm()) and
      Assigned(FAppModules.MainForm.PageControl) then
    begin
      if ((Assigned(FIFRTabSheetManager)) and
        (Assigned(FIFRTabSheetManager.TabSheet)) and
        (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FIFRTabSheetManager.TabSheet)) then
        Result := FIFRTabSheetManager.DoCustomTabSheetEvent(AData);
    end;
    if not Result then
      Result := inherited ProcessCustomModelEvent(AData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRModelManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TIFRModelManager.ProcessEvent';
begin
  Result := False;
  try
    Result := True;
    case AEventType of
      CmeCreateIFRSite : DoCreateIFRSite('');
      CmeDeleteIFRSite : DoDeleteIFRSite(NullInteger);
      CmeSaveIFRSite   : DoSaveIFRSite(NullInteger);
      CmeIFRUserGuide    : DoLaunchIFRUserGuide;
    else
      Result := inherited ProcessEvent(AEventType, AData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRModelManager.StudyDataHasChanged( AContext: TChangeContext;
                                                        AFieldName, AOldValue,
                                                        ANewValue: String ): Boolean;
const OPNAME = 'TIFRModelManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Assigned(FIFRTabSheetManager) then
     FIFRTabSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FModelGUIManager) then
     FModelGUIManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FMenuItemManager) and (AContext = sdccEdit) then
      FMenuItemManager.SetSaveIFRSite(True);
    Result := True;
  except on E: Exception do HandleError( E, OPNAME ); end;
end;

function TIFRModelManager.StudyHasChanged: Boolean;
const OPNAME = 'TIFRModelManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    Result := LoadModelData;
    if Assigned(FIFRTabSheetManager) then
      Result := Result and FIFRTabSheetManager.StudyHasChanged;
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.StudyHasChanged;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelManager.ViewInputDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): Boolean;
const OPNAME = 'TIFRModelManager.ViewInputDialog';
var
  LDataList   : TStringList;
  LIdentifier : integer;
begin
  Result := False;
  try
    FMenuItemManager.SetCreateIFRSite(True);
    FMenuItemManager.SetDeleteIFRSite(False);
    FMenuItemManager.SetSaveIFRSite(False);

    Result := FModelGUIManager.ViewInputDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));

    if Assigned(FIFRTabSheetManager) and Assigned(FMenuItemManager) and
      (TTreeViewTabSheet(FIFRTabSheetManager.TabSheet).TreeView.Selected <> nil) and
      (TTreeViewTabSheet(FIFRTabSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      LDataList := TStringList.Create;
      try
        LDataList.CommaText := ACommaTextContextData;
        LIdentifier := StrToInt(LDataList.Values['MODELELEMENTID']);
        if(FModelData.IFRSiteDataList.IFRSiteDataByIdentifier[LIdentifier] <> nil) then
        begin
          FMenuItemManager.SetCreateIFRSite(True);
          FMenuItemManager.SetDeleteIFRSite(True);
          FMenuItemManager.SetSaveIFRSite(False);
        end;
      finally
        FreeAndNil(LDataList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelManager.AddAndSelectElement (AViewID : string; AParentID : string; AWeighting : integer;
                                               ACaption : string; ABitmapName : string; ADataType: string;
                                               ASelect : boolean = True) : boolean;
const OPNAME = 'TIFRModelManager.AddAndSelectElement';
var
  LViewDataNode : TViewDataNode;
  LTreeView     : TTreeView;
  LTreeNodes    : TTreeNodes;
  LTreeNode     : TTreeNode;
  LNewTreeNode  : TTreeNode;
  LParentNode   : TTreeNode;
  LTreeNodeData : TViewDataTreeNodeData;
  LIndex        : integer;
  LFound        : boolean;
  LImageList    : IStringImageList;
begin
  Result := False;
  try
    if (FAppModules.MainForm = nil)  then
      Result := True
    else
    begin
      LViewDataNode := TViewDataNode.Create;
      LViewDataNode.ViewID          := AViewID;
      LViewDataNode.ParentID        := AParentID;
      LViewDataNode.Weighting       := AWeighting;
      LViewDataNode.OverrideCaption := ACaption;
      LViewDataNode.BitmapName      := ABitmapName;
      LViewDataNode.DataType        := ADataType;
      if (FViewDataManager.ViewDataList.AddViewDataNode(LViewDataNode)) then
      begin
        LTreeView   := TTreeViewTabSheet(FIFRTabSheetManager.TabSheet).TreeView;
        LTreeNodes  := LTreeView.Items;
        LIndex      := 0;
        LParentNode := nil;
        while ((not Assigned(LParentNode)) and (LIndex < LTreeNodes.Count)) do
        begin
          if (UpperCase(TViewDataTreeNodeData(LTreeNodes[LIndex].Data).ViewDataNode.ViewID) = UpperCase(AParentID)) then
            LParentNode := LTreeNodes[LIndex]
          else
            LIndex := LIndex + 1;
        end;
        if (Assigned(LParentNode)) then
        begin
          LTreeNodeData := TViewDataTreeNodeData.Create;
          LTreeNodeData.ViewDataNode := lViewDataNode;
          LTreeNode := lParentNode.GetFirstChild;
          LFound    := False;
          while ((not LFound) and Assigned(LTreeNode)) do
          begin
            if (ADataType = 'IFRSITEDATA') then
            begin
              if (TViewDataTreeNodeData(lTreeNode.Data).ViewDataNode.Weighting > AWeighting) then
                LFound := True
              else
                LTreeNode := LParentNode.GetNextChild(LTreeNode);
            end
            else
            begin
              if (TViewDataTreeNodeData(LTreeNode.Data).ViewDataNode.OverrideCaption > ACaption) then
                LFound := True
              else
                LTreeNode := LParentNode.GetNextChild(LTreeNode);
            end;
          end;
          if (LFound) then
            LNewTreeNode := LTreeNodes.InsertObject(LTreeNode, ACaption, LTreeNodeData)
          else
            LNewTreeNode := LTreeNodes.AddChildObject(LParentNode, ACaption, LTreeNodeData);
          if (LTreeNodes.Owner.GetInterface(IStringImageList, LImageList)) then
            LNewTreeNode.StateIndex := LImageList.AddResourceBitmap(ABitmapName);
          LTreeNodeData.TreeNode := LNewTreeNode;
          if (ASelect) then
            LTreeView.Selected := LNewTreeNode;
          Result := True;
        end
        else
        begin
          FViewDataManager.ViewDataList.DeleteViewDataNode(LViewDataNode);
        end;
      end
      else
        FreeAndNil(LViewDataNode);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRModelManager.DeleteElement(ATreeNode : TTreeNode) : boolean;
const OPNAME = 'TIFRModelManager.DeleteElement';
var
  LViewDataNode : TViewDataNode;
  LTreeNodeData : TViewDataTreeNodeData;
  LParentNode   : TTreeNode;
  LPrevSibling  : TTreeNode;
  LNextSibling  : TTreeNode;
  LTreeView     : TTreeview;
begin
  Result := FALSE;
  try
    if (FAppModules.MainForm = nil)  then
      Result := True
    else
    begin
      LTreeNodeData := TViewDataTreeNodeData(ATreeNode.Data);
      LViewDataNode := LTreeNodeData.ViewDataNode;
      if (FViewDataManager.ViewDataList.DeleteViewDataNode(LViewDataNode)) then
      begin
        if (Assigned(LTreeNodeData)) then
          FreeAndNil(LTreeNodeData);
        LParentNode  := ATreeNode.Parent;
        LPrevSibling := LParentNode.GetPrevChild(ATreeNode);
        LNextSibling := LParentNode.GetNextChild(ATreeNode);

        LTreeView    := TTreeViewTabSheet(FIFRTabSheetManager.TabSheet).TreeView;
        LTreeView.Items.Delete(ATreeNode);
        if (Assigned(LNextSibling)) then
          LTreeView.Selected := LNextSibling
        else if (Assigned(LPrevSibling)) then
          LTreeView.Selected := LPrevSibling
        else
          LTreeView.Selected := LParentNode;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRModelManager.DoCreateIFRSite(ACSVFileName: string): TIFRSiteDataObject;
const OPNAME = 'TIFRModelManager.DoCreateIFRSite';
var
  LIFRSiteData : TIFRSiteDataObject;
  LFileSelector: TOpenDialog;
  LLoadAgent:TIFRDataLoadAgent;
begin
  Result := nil;
  try
    ACSVFileName := Trim(ACSVFileName);
    if(ACSVFileName = '') or (not FileExists(ACSVFileName)) then
    begin
      LFileSelector := TOpenDialog.Create(nil);
      try
        LFileSelector.Filter  := 'All Files|*.*|(*.CSV)|*.csv';
        LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
        if LFileSelector.Execute then
        begin
          ACSVFileName := LFileSelector.FileName;
        end;
      finally
        LFileSelector.Free;
      end;
    end;

    if (ACSVFileName = '') then Exit;

    LLoadAgent := TIFRDataLoadAgent.Create(FAppModules);
    try
      if not LLoadAgent.ValidateCSVFile(ACSVFileName) then
        Exit;
      LIFRSiteData := FModelData.IFRSiteDataList.CreateIFRSite(ACSVFileName);
      if LIFRSiteData <> nil then
      begin
        LLoadAgent.LoadDataFromFile(ACSVFileName,LIFRSiteData);
        AddAndSelectElement('IFRSITEDATA',
                            'IFRSITEDATA',
                            LIFRSiteData.SiteIdentifier,
                            LIFRSiteData.SiteName,
                            'SMALLCLOCK',
                            'IFRSITEDATA');
         StudyDataHasChanged(sdccAdd, 'IFRSite', '', IntToStr(LIFRSiteData.SiteIdentifier));
         if(FMenuItemManager <> nil) then
          FMenuItemManager.SetSaveIFRSite(True);
      end;
      Result := LIFRSiteData;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TIFRModelManager.DoDeleteIFRSite(AIFRSiteID: integer): boolean;
const OPNAME = 'TIFRModelManager.DoDeleteIFRSite';
var
 LNode : TTreeNode;
begin
  Result := False;
  try
    if(AIFRSiteID = NullInteger) then
    begin
      if Assigned(FIFRTabSheetManager) then
      begin
        LNode := TTreeViewTabSheet(FIFRTabSheetManager.TabSheet).TreeView.Selected;
        if(LNode <> nil) and (LNode.Data <> nil) and (LNode.Level = 1) then
        begin
          AIFRSiteID := TViewDataTreeNodeData(LNode.Data).ViewDataNode.Weighting;
        end;
      end;
    end;

    Result := FModelData.IFRSiteDataList.DeleteIFRSite(AIFRSiteID);
    if Result then
    begin
      if Assigned(FIFRTabSheetManager) then
      begin
        FMenuItemManager.SetDeleteIFRSite(False);
        FMenuItemManager.SetSaveIFRSite(False);
        DeleteElement(TTreeViewTabSheet(FIFRTabSheetManager.TabSheet).TreeView.Selected);
      end;
      StudyDataHasChanged(sdccDelete, 'IFRSite', '', IntToStr(AIFRSiteID));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRModelManager.DoSaveIFRSite(AIFRSiteID: integer): boolean;
const OPNAME = 'TIFRModelManager.DoSaveIFRSite';
var
 LNode : TTreeNode;
begin
  Result := False;
  try
    if(AIFRSiteID = NullInteger) then
    begin
      if Assigned(FIFRTabSheetManager) then
      begin
        LNode := TTreeViewTabSheet(FIFRTabSheetManager.TabSheet).TreeView.Selected;
        if(LNode <> nil) and (LNode.Data <> nil) and (LNode.Level = 1) then
        begin
          AIFRSiteID := TViewDataTreeNodeData(LNode.Data).ViewDataNode.Weighting;
        end;
      end;
    end;

    Result := FModelData.IFRSiteDataList.SaveIFRSite(AIFRSiteID);

    if Result then
    begin
      if Assigned(FMenuItemManager) then
      begin
        FMenuItemManager.SetSaveIFRSite(False);
      end;
      StudyDataHasChanged(sdccSaveData, 'IFRSite', '', IntToStr(AIFRSiteID));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRModelManager.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TIFRModelManager.Validate';
begin
  Result := False;
  try
    Result := FModelData.Validate(AErrors,AContext);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TIFRModelManager.LoadModelData: boolean;
const OPNAME = 'TIFRModelManager.LoadModelData';
var
  LAgent : TIFRDataLoadAgent;
begin
  Result := False;
  try
    LAgent := TIFRDataLoadAgent.Create(FAppModules);
    try
      FModelData.Initialise;
      Result := LAgent.LoadDataFromDB(FModelData.IFRSiteDataList);
    finally
      LAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TIFRModelManager.DoLaunchIFRUserGuide;
const OPNAME = 'TIFRModelManager.DoLaunchIFRUserGuide';
var
  LFileName : string;
begin
  try
    if IsAcrobatReaderInstalled then
    begin
      LFileName := ExtractFilePath(ApplicationExeName);
      TPDFDocumentLauncher.Launch(LFileName + 'help\IFR USER GUIDE_WRMF4.0_FinalDRAFT.pdf');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
