//
//
//  UNIT      : Contains TSedimentationModelManager Class
//  AUTHOR    : Sam Dhlamini (BCX)
//  DATE      : 13/02/2017
//  COPYRIGHT : Copyright © 2017 DWA
//
//
unit USedimentationModelManager;

interface

uses

  Contnrs,
  Classes,
  VCL.ComCtrls,
  UAbstractObject,
  UGenericModelLinkClasses,
  USystemModelManager,
  UTabSheetManager,

  UDamSedimentationDataObject,
  USedimentationGUIManager,
  USedimentationMenuItemManager,
  USedimentationTabSheetManager,
  VoaimsCom_TLB;

type

  TSedimentationModelManager = class(TSystemModelManager)
  protected
    FModelData                             : TDamSedimentationModelData;
    FSedimentationSheetManager             : TSedimentationTabSheetManager;
    FMenuItemManager                       : TSedimentationMenuItemManager;
    FModelGUIManager                       : TSedimentationGUIManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure RefreshMenuItems; override;
    procedure DoLaunchSedimentationUserGuide;

    function DoCreateDamSedimentation(ASurveyFileName: string): TDamSedimentationDataObject;
    function DoDeleteDamSedimentation(ADamSedimentationID: integer): boolean;
    function DoSaveDamSedimentation(ADamSedimentationID: integer): boolean;

    function DeleteElement(ATreeNode : TTreeNode) : boolean;
    function AddAndSelectElement(AViewID : string; AParentID : string; AWeighting : integer;
                                 ACaption : string; ABitmapName : string; ADataType: string;
                                 ASelect : boolean = True) : boolean;
  public
    function ModelName: string; override;
    function LoadModelData: boolean;
    function Initialise: boolean; override;
    function PopulateDataList(AModelDataChangeType: TModelDataChangeType; AList: TList): Boolean; override;
    function ViewInputDialog(AParent: TObject; ACommaTextContextData: String;AOwner: TObject = nil): Boolean; override;
    function DeleteModelDataViewer(AParent: TObject; AOwner: TObject): Boolean; override;
    function LanguageHasChanged: Boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : string;
                                  AOldValue  : string;
                                  ANewValue  : string): Boolean; override;
    function StudyHasChanged: Boolean; override;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; override;
    function ProcessCustomModelEvent(AData: TModelMenuData): boolean; override;
    function ProcessParameterChangeEvent : boolean; override;
    function ProcessMetaDataEvent : boolean; override;
    //function DoExportDamSedimentationData: boolean;
    function DoRefreshFileHints: boolean;
    function RefreshModelData: boolean;
    function ReselectElement(ATreeNode : TTreeNode) : boolean;
    function ModelData: TInterfacedObject; override;
    procedure OnTabHasChanged(ASender: TObject); override;

    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;

    property MenuItemManager :  TSedimentationMenuItemManager read FMenuItemManager;

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
  USedimentationDataLoadAgent,
  UIsAcrobatReaderInstalled,
  UPDFDocumentLauncher,
  UErrorHandlingOperations, Math;

procedure TSedimentationModelManager.CreateMemberObjects;
const OPNAME = 'TSedimentationModelManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FModelData := TDamSedimentationModelData.Create(FAppModules);
    if Assigned(FAppModules.MainForm()) and
      Assigned(FAppModules.MainForm.PageControl) then
    begin
      FMenuItemManager := TSedimentationMenuItemManager.Create(FAppModules);
      FSedimentationSheetManager := TSedimentationTabSheetManager.Create(FAppModules);
      FSedimentationSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
      FModelGUIManager := TSedimentationGUIManager.Create(FAppModules);

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSedimentationModelManager.DeleteModelDataViewer(AParent,
  AOwner: TObject): Boolean;
const OPNAME = 'TSedimentationModelManager.DeleteModelDataViewer';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSedimentationModelManager.DestroyMemberObjects;
const OPNAME = 'TSedimentationModelManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    if Assigned(FSedimentationSheetManager) then
      FreeAndNil(FSedimentationSheetManager);
    if Assigned(FMenuItemManager) then
      FreeAndNil(FMenuItemManager);
    if Assigned(FModelData) then
      FreeAndNil(FModelData);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TSedimentationModelManager.DoRefreshFileHints: boolean;
const OPNAME = 'TSedimentationModelManager.DoRefreshFileHints';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationModelManager.Initialise: boolean;
const OPNAME = 'TSedimentationModelManager.Initialise';
begin
  Result := inherited Initialise;
  try
    if Assigned(FModelData) then
      FModelData.Initialise;
    if Assigned(FSedimentationSheetManager) then
      Result := Result and FSedimentationSheetManager.Initialise;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSedimentationModelManager.LanguageHasChanged: Boolean;
const OPNAME = 'TSedimentationModelManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned (FSedimentationSheetManager) then
      Result := Result and FSedimentationSheetManager.LanguageHasChanged;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSedimentationModelManager.ModelData: TInterfacedObject;
const OPNAME = 'TSedimentationModelManager.ModelData';
begin
  Result := nil;
  try
    Result := FModelData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationModelManager.ModelName: string;
const OPNAME = 'TSedimentationModelManager.ModelName';
begin
  Result := '';
  try
    Result := CDamSedimentation;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationModelManager.LoadModelData: boolean;
const OPNAME = 'TSedimentationModelManager.LoadModelData';
//var
//  LAgent : TSedimentationDataLoadAgent;
begin
  Result := False;
  try
  //  LAgent := TSedimentationDataLoadAgent.Create(FAppModules);
    try
      FModelData.Initialise;
    //  Result := LAgent.LoadDataFromDB(FModelData.SedimentationDataList);
    finally
      //LAgent.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSedimentationModelManager.OnTabHasChanged(ASender: TObject);
const OPNAME = 'TSedimentationModelManager.OnTabHasChanged';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSedimentationModelManager.PopulateDataList(
  AModelDataChangeType: TModelDataChangeType; AList: TList): Boolean;
const OPNAME = 'TSedimentationModelManager.PopulateDataList';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSedimentationModelManager.ProcessCustomModelEvent (AData : TModelMenuData) : boolean;
const OPNAME = 'TSedimentationModelManager.ProcessCustomModelEvent';
begin
  Result := False;
  try
    if Assigned(FAppModules.MainForm()) and
      Assigned(FAppModules.MainForm.PageControl) then
    begin
      if ((Assigned(FSedimentationSheetManager )) and
        (Assigned(FSedimentationSheetManager.TabSheet)) and
        (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FSedimentationSheetManager.TabSheet)) then
        Result := FSedimentationSheetManager.DoCustomTabSheetEvent(AData);
    end;
    if not Result then
      Result := inherited ProcessCustomModelEvent(AData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationModelManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TSedimentationModelManager.ProcessEvent';
begin
  Result := False;
  try
    Result := True;
    case AEventType of
      CmeCreateDamSedimentation : DoCreateDamSedimentation('');
      CmeDeleteDamSedimentation : DoDeleteDamSedimentation(NullInteger);
      CmeSaveDamSedimentation   : DoSaveDamSedimentation(NullInteger);
      CmeSediUserGuide    : DoLaunchSedimentationUserGuide;
    else
      Result := inherited ProcessEvent(AEventType, AData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationModelManager.ReselectElement(ATreeNode : TTreeNode) : boolean;
const OPNAME = 'TSedimentationModelManager.ReselectElement';
var
  LTreeView     : TTreeview;
begin
  Result := False;
  try
    if (FAppModules.MainForm = nil)  then
      Result := True
    else
    begin
      LTreeView := TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView;
      LTreeView.Selected := nil;
      LTreeView.Selected := ATreeNode;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TSedimentationModelManager.RefreshMenuItems;
const OPNAME = 'TSedimentationModelManager.RefreshMenuItems';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationModelManager.StudyDataHasChanged( AContext: TChangeContext;
                                                        AFieldName, AOldValue,
                                                        ANewValue: String ): Boolean;
const OPNAME = 'TSedimentationModelManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Assigned(FSedimentationSheetManager) then
      Result := Result and FSedimentationSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);

  except on E: Exception do HandleError( E, OPNAME ); end;
end;

function TSedimentationModelManager.StudyHasChanged: Boolean;
const OPNAME = 'TSedimentationModelManager.StudyHasChanged';
begin
  Result := False;
  try
    if Assigned(FModelData) then
    begin
      Result := FModelData.LoadData;

    end;
    Result := Result and inherited StudyHasChanged;

    if Assigned(FSedimentationSheetManager) then
      Result := Result and FSedimentationSheetManager.StudyHasChanged;
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.StudyHasChanged;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSedimentationModelManager.ViewInputDialog(AParent: TObject;
  ACommaTextContextData: String; AOwner: TObject): Boolean;
const OPNAME = 'TSedimentationModelManager.ViewInputDialog';
var
  LDataList   : TStringList;
  //LViewType   : string;
  LIdentifier : integer;
begin
    Result := False;
  try
    FMenuItemManager.SetCreateDamSedimentation(True);
    FMenuItemManager.SetDeleteDamSedimentation(False);
    FMenuItemManager.SetSaveDamSedimentation(False);

    Result := FModelGUIManager.ViewInputDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));

    if Assigned(FSedimentationSheetManager) and Assigned(FMenuItemManager) and
      (TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView.Selected <> nil) and
      (TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      LDataList := TStringList.Create;
      try
        LDataList.CommaText := ACommaTextContextData;
        LIdentifier := StrToInt(LDataList.Values['MODELELEMENTID']);
        if(FModelData.DamSedimentationDataList.DamSedimentationDataByIdentifier[LIdentifier] <> nil) then
        begin
          FMenuItemManager.SetCreateDamSedimentation(True);
          FMenuItemManager.SetDeleteDamSedimentation(True);
          FMenuItemManager.SetSaveDamSedimentation(False);
        end;
      finally
        FreeAndNil(LDataList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSedimentationModelManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TSedimentationModelManager.ProcessParameterChangeEvent';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationModelManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TSedimentationModelManager.ProcessMetaDataEvent';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TSedimentationModelManager.DoDeleteDamSedimentation(ADamSedimentationID: integer): boolean;
const OPNAME = 'TSedimentationModelManager.DoDeleteDamSedimentation';
var
 LNode : TTreeNode;
begin
  Result := False;
  try
    if(ADamSedimentationID = NullInteger) then
    begin
      if Assigned(FSedimentationSheetManager) then
      begin
        LNode := TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView.Selected;
        if(LNode <> nil) and (LNode.Data <> nil) and (LNode.Level = 1) then
        begin
          ADamSedimentationID := TViewDataTreeNodeData(LNode.Data).ViewDataNode.Weighting;
        end;
      end;
    end;

    Result := FModelData.DamSedimentationDataList.DeleteDamSedimentation(ADamSedimentationID);
    if Result then
    begin
      if Assigned(FSedimentationSheetManager) then
      begin
        FMenuItemManager.SetDeleteDamSedimentation(False);
        FMenuItemManager.SetSaveDamSedimentation(False);
        DeleteElement(TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView.Selected);
      end;
      StudyDataHasChanged(sdccDelete, 'DamSedimentation', '', IntToStr(ADamSedimentationID));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationModelManager.DoSaveDamSedimentation(ADamSedimentationID: integer): boolean;
const OPNAME = 'TSedimentationModelManager.DoSaveDamSedimentation';
var
 LNode : TTreeNode;
begin
  Result := False;
  try
    if(ADamSedimentationID = NullInteger) then
    begin
      if Assigned(FSedimentationSheetManager) then
      begin
        LNode := TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView.Selected;
        if(LNode <> nil) and (LNode.Data <> nil) and (LNode.Level = 1) then
        begin
          ADamSedimentationID := TViewDataTreeNodeData(LNode.Data).ViewDataNode.Weighting;
        end;
      end;
    end;

    Result := FModelData.DamSedimentationDataList.SaveDamSedimentation(ADamSedimentationID);

    if Result then
    begin
      if Assigned(FMenuItemManager) then
      begin
        FMenuItemManager.SetSaveDamSedimentation(False);
      end;
      StudyDataHasChanged(sdccSaveData, 'DamSedimentation', '', IntToStr(ADamSedimentationID));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TSedimentationModelManager.DoCreateDamSedimentation(ASurveyFileName: string): TDamSedimentationDataObject;
const OPNAME = 'TSedimentationModelManager.DoCreateDamSedimentation';
var
  LDamSedimentationData : TDamSedimentationDataObject;
  LFileSelector: TOpenDialog;
  LLoadAgent: TSedimentationDataLoadAgent;
begin
  Result := nil;
  try
    ASurveyFileName := Trim(ASurveyFileName);
    if(ASurveyFileName = '') or (not FileExists(ASurveyFileName)) then
    begin
      LFileSelector := TOpenDialog.Create(nil);
      try
        LFileSelector.Filter  := 'All Files|*.*|(*.CSV)|*.csv';
        LFileSelector.Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
        if LFileSelector.Execute then
        begin
          ASurveyFileName := LFileSelector.FileName;
        end;
      finally
        LFileSelector.Free;
      end;
    end;

    if (ASurveyFileName = '') then Exit;

    LLoadAgent := TSedimentationDataLoadAgent.Create(FAppModules);
    try
      if not LLoadAgent.ValidateSurveyFile(ASurveyFileName) then
        Exit;
      LDamSedimentationData := FModelData.DamSedimentationDataList.CreateDamSedimentation(ASurveyFileName);
      if LDamSedimentationData <> nil then
      begin
        LLoadAgent.LoadDataFromFile(ASurveyFileName,FModelData.DamSedimentationDataList);
        AddAndSelectElement('DAMSEDIMENTATIONDATA',
                            'DAMSEDIMENTATIONDATA',
                            LDamSedimentationData.DamIdentifier,
                            LDamSedimentationData.DamName,
                            'SMALLCLOCK',
                            'DAMSEDIMENTATIONDATA');
         StudyDataHasChanged(sdccAdd, 'DamSedimentation', '', IntToStr(LDamSedimentationData.DamIdentifier));
         if(FMenuItemManager <> nil) then
          FMenuItemManager.SetSaveDamSedimentation(True);
      end;
      Result := LDamSedimentationData;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TSedimentationModelManager.AddAndSelectElement (AViewID : string; AParentID : string; AWeighting : integer;
                                                          ACaption : string; ABitmapName : string; ADataType: string;
                                                          ASelect : boolean = True) : boolean;
const OPNAME = 'TSedimentationModelManager.AddAndSelectElement';
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
        LTreeView   := TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView;
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
            if (ADataType = 'DAMSEDIMENTATIONDATA') then
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

function TSedimentationModelManager.DeleteElement(ATreeNode : TTreeNode) : boolean;
const OPNAME = 'TSedimentationModelManager.DeleteElement';
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

        LTreeView    := TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView;
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

function TSedimentationModelManager.RefreshModelData: boolean;
const OPNAME = 'TSedimentationModelManager.RefreshModelData';
begin
  Result :=False;
  try
    Initialise;
    StudyHasChanged;
    LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 (*
function TSedimentationModelManager.DoExportDamSedimentationData: boolean;
const OPNAME = 'TSedimentationModelManager.DoExportDamSedimentationData';
var
  LDamID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FSedimentationSheetManager.TabSheet).TreeView.Selected.Data);
      LDamID := NullInteger;
      if LDataObject <> nil then
        LDamID := LDataObject.ViewDataNode.Weighting;
      if LDamID <> NullInteger then
        Result := FModelData.ExportDamSedimentationData(LDamID);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

   *)
function TSedimentationModelManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TSedimentationModelManager.CanCopyToCLipboard';
begin
  Result := False;
  try
    if Assigned(FModelGUIManager) then
      Result := FModelGUIManager.CanCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationModelManager.CanExport: boolean;
const OPNAME = 'TSedimentationModelManager.CanExport';
begin
  Result := False;
  try
    if Assigned(FModelGUIManager) then
      Result := FModelGUIManager.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSedimentationModelManager.CanPrint: boolean;
const OPNAME = 'TSedimentationModelManager.CanPrint';
begin
  Result := False;
  try
    if Assigned(FModelGUIManager) then
      Result := FModelGUIManager.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSedimentationModelManager.DoCopyToCLipboard;
const OPNAME = 'TSedimentationModelManager.DoCopyToCLipboard';
begin
  try
    if Assigned(FModelGUIManager) then
      FModelGUIManager.DoCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSedimentationModelManager.DoExport(AFileName: string);
const OPNAME = 'TSedimentationModelManager.DoExport';
begin
  try
    if Assigned(FModelGUIManager) then
      FModelGUIManager.DoExport;//(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSedimentationModelManager.DoPrint;
const OPNAME = 'TSedimentationModelManager.DoPrint';
begin
  try
    if Assigned(FModelGUIManager) then
      FModelGUIManager.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSedimentationModelManager.DoLaunchSedimentationUserGuide;
const OPNAME = 'TSedimentationModelManager.DoLaunchSedimentationUserGuide';
var
  LFileName : string;
begin
  try
    if IsAcrobatReaderInstalled then
    begin
      LFileName := ExtractFilePath(ApplicationExeName);
      TPDFDocumentLauncher.Launch(LFileName + 'help\DailyDiv USER GUIDE_WRMF4.0_FinalDRAFT.pdf');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
