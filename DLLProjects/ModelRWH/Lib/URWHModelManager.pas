//
//
//  UNIT      : Contains TRWHModelManager Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//
unit URWHModelManager;

interface

uses

  Contnrs,
  Classes,
  VCL.ComCtrls,
  UAbstractObject,
  UGenericModelLinkClasses,
  USystemModelManager,
  URWHDataObject,

  URWHGaugeSelectionTabSheetManager,
  UViewGaugeDataTabSheetManager,
  URunConfigurationTabSheetManager,
  URunModelTabSheetManager,
  UViewOutputTabSheetManager,
  UViewContoursTabSheetManager,
  UWeatherEventsTabSheetManager,

  URWHGUIManager,
  URWHMenuItemManager;

type

  TRWHModelManager = class(TSystemModelManager)
  protected
    FModelData                             : TRWHModelData;
    FMenuItemManager                       : RWHMenuItemManager;
    FModelGUIManager                       : TRWHGUIManager;

    FGaugeSelectionTabSheetManager         : TRWHGaugeSelectionTabSheetManager;
    FViewGaugeDataTabSheetManager          : TViewGaugeDataTabSheetManager;
    FRunConfigurationTabSheetManager       : TRunConfigurationTabSheetManager;
    FRunModelTabSheetManager               : TRunModelTabSheetManager;
    FViewOutputTabSheetManager             : TViewOutputTabSheetManager;
    FViewContoursTabSheetManager           : TViewContoursTabSheetManager;
    FWeatherEventsTabSheetManager          : TWeatherEventsTabSheetManager;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    //function AddAndSelectElement(AViewID : string; AParentID : string; AWeighting : integer; ACaption : string;
    //         ABitmapName : string; ADataType: string; ASelect : boolean = True): boolean;
    //function DeleteElement(ATreeNode : TTreeNode) : boolean;
    procedure DoLaunchRWHUserGuide;
    procedure LoadModelData;
  public
    //function DoCreateRWHSite(ACSVFileName: string): TRWHSiteDataObject;
    //function DoDeleteRWHSite(ARWHSiteID: integer): boolean;
    //function DoSaveRWHSite(ARWHSiteID: integer): boolean;

    function ModelName: string; override;
    function Initialise: boolean; override;
    function Validate(var AErrors: WideString; const AContext: WideString): WordBool; safecall;
    function ViewInputDialog(AParent: TObject; ACommaTextContextData: String;AOwner: TObject = nil): Boolean; override;
    function LanguageHasChanged: Boolean; override;
    function StudyDataHasChanged (AContext   : TChangeContext;AFieldName : string; AOldValue  : string;  ANewValue  : string): Boolean; override;
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
  URWHDataLoadAgent,
  UIsAcrobatReaderInstalled,
  UPDFDocumentLauncher,
  UErrorHandlingOperations, Math;

procedure TRWHModelManager.CreateMemberObjects;
const OPNAME = 'TRWHModelManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    RWHModelData := TRWHModelData.Create(FAppModules);
    FModelData   := RWHModelData;
    if Assigned(FAppModules.MainForm()) and Assigned(FAppModules.MainForm.PageControl) then
    begin
      FMenuItemManager := RWHMenuItemManager.Create(FAppModules);

      FGaugeSelectionTabSheetManager                      := TRWHGaugeSelectionTabSheetManager.Create(FAppModules);
      FGaugeSelectionTabSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
      FViewGaugeDataTabSheetManager                         := TViewGaugeDataTabSheetManager.Create(FAppModules);
      FViewGaugeDataTabSheetManager.TabSheet.PageControl    := FAppModules.MainForm.PageControl;
      FRunConfigurationTabSheetManager                      := TRunConfigurationTabSheetManager.Create(FAppModules);
      FRunConfigurationTabSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
      FRunModelTabSheetManager                              := TRunModelTabSheetManager.Create(FAppModules);
      FRunModelTabSheetManager.TabSheet.PageControl         := FAppModules.MainForm.PageControl;
      FViewOutputTabSheetManager                            := TViewOutputTabSheetManager.Create(FAppModules);
      FViewOutputTabSheetManager.TabSheet.PageControl       := FAppModules.MainForm.PageControl;
      FViewContoursTabSheetManager                          := TViewContoursTabSheetManager.Create(FAppModules);
      FViewContoursTabSheetManager.TabSheet.PageControl     := FAppModules.MainForm.PageControl;
      FWeatherEventsTabSheetManager                         := TWeatherEventsTabSheetManager.Create(FAppModules);
      FWeatherEventsTabSheetManager.TabSheet.PageControl    := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRWHModelManager.DestroyMemberObjects;
const OPNAME = 'TRWHModelManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    if Assigned(FModelData) then
      FreeAndNil(FModelData);
    if Assigned(FMenuItemManager) then
      FreeAndNil(FMenuItemManager);
    if Assigned(FModelGUIManager) then
      FreeAndNil(FModelGUIManager);

    if Assigned(FGaugeSelectionTabSheetManager) then
      FreeAndNil(FGaugeSelectionTabSheetManager);
    if Assigned(FViewGaugeDataTabSheetManager) then
      FreeAndNil(FViewGaugeDataTabSheetManager);
    if Assigned(FRunConfigurationTabSheetManager) then
      FreeAndNil(FRunConfigurationTabSheetManager);
    if Assigned(FRunModelTabSheetManager) then
      FreeAndNil(FRunModelTabSheetManager);
    if Assigned(FViewOutputTabSheetManager) then
      FreeAndNil(FViewOutputTabSheetManager);
    if Assigned(FViewContoursTabSheetManager) then
      FreeAndNil(FViewContoursTabSheetManager);
    if Assigned(FWeatherEventsTabSheetManager) then
      FreeAndNil(FWeatherEventsTabSheetManager);

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelManager.Initialise: boolean;
const OPNAME = 'TRWHModelManager.Initialise';
begin
  Result := inherited Initialise;
  try
    if Assigned(FModelData) then
      FModelData.Initialise;

    if Assigned(FMenuItemManager) then
      FMenuItemManager.Initialise;
    if Assigned(FModelGUIManager) then
      FModelGUIManager.Initialise;
   if Assigned(FGaugeSelectionTabSheetManager) then
      FGaugeSelectionTabSheetManager.Initialise;
    if Assigned(FViewGaugeDataTabSheetManager) then
      FViewGaugeDataTabSheetManager.Initialise;
    if Assigned(FRunConfigurationTabSheetManager) then
      FRunConfigurationTabSheetManager.Initialise;
    if Assigned(FRunModelTabSheetManager) then
      FRunModelTabSheetManager.Initialise;
    if Assigned(FViewOutputTabSheetManager) then
      FViewOutputTabSheetManager.Initialise;
    if Assigned(FViewContoursTabSheetManager) then
      FViewContoursTabSheetManager.Initialise;
    if Assigned(FWeatherEventsTabSheetManager) then
      FWeatherEventsTabSheetManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelManager.LanguageHasChanged: Boolean;
const OPNAME = 'TRWHModelManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try

    if Assigned (FModelData) then
      FModelData.LanguageHasChanged;
    if Assigned(FMenuItemManager) then
      FMenuItemManager.LanguageHasChanged;
    if Assigned (FModelGUIManager) then
      FModelGUIManager.LanguageHasChanged;

    if Assigned (FGaugeSelectionTabSheetManager) then
      FGaugeSelectionTabSheetManager.LanguageHasChanged;
    if Assigned (FViewGaugeDataTabSheetManager) then
      FViewGaugeDataTabSheetManager.LanguageHasChanged;
    if Assigned (FRunConfigurationTabSheetManager) then
      FRunConfigurationTabSheetManager.LanguageHasChanged;
    if Assigned (FRunModelTabSheetManager) then
      FRunModelTabSheetManager.LanguageHasChanged;
    if Assigned (FViewOutputTabSheetManager) then
      FViewOutputTabSheetManager.LanguageHasChanged;
    if Assigned (FViewContoursTabSheetManager) then
      FViewContoursTabSheetManager.LanguageHasChanged;
    if Assigned (FWeatherEventsTabSheetManager) then
      FWeatherEventsTabSheetManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRWHModelManager.LoadModelData;
const OPNAME = 'TRWHModelManager.StudyHasChanged';
var
  LDataLoadAgent : TRWHDataLoadAgent;
begin
  try
    LDataLoadAgent := TRWHDataLoadAgent.Create(FAppModules);
    try
      LDataLoadAgent.LoadRWHData(FModelData);
    finally
      LDataLoadAgent.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelManager.StudyHasChanged: Boolean;
const OPNAME = 'TRWHModelManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    LoadModelData;

    if Assigned(FMenuItemManager) then
      FMenuItemManager.StudyHasChanged;
    if Assigned (FModelGUIManager) then
      FModelGUIManager.StudyHasChanged;

    if Assigned (FGaugeSelectionTabSheetManager) then
      FGaugeSelectionTabSheetManager.StudyHasChanged;
    if Assigned (FViewGaugeDataTabSheetManager) then
      FViewGaugeDataTabSheetManager.StudyHasChanged;
    if Assigned (FRunConfigurationTabSheetManager) then
      FRunConfigurationTabSheetManager.StudyHasChanged;
    if Assigned (FRunModelTabSheetManager) then
      FRunModelTabSheetManager.StudyHasChanged;
    if Assigned (FViewOutputTabSheetManager) then
      FViewOutputTabSheetManager.StudyHasChanged;
    if Assigned (FViewContoursTabSheetManager) then
      FViewContoursTabSheetManager.StudyHasChanged;
    if Assigned (FWeatherEventsTabSheetManager) then
      FWeatherEventsTabSheetManager.StudyHasChanged;

    {if Assigned(FGaugeSelectionTabSheetManager) then
    begin
      FGaugeSelectionTabSheetManager.TabSheet.PageIndex := 0;
      FAppModules.MainForm.PageControl.ActivePage := FGaugeSelectionTabSheetManager.TabSheet;
    end;}

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRWHModelManager.ModelData: TInterfacedObject;
const OPNAME = 'TRWHModelManager.ModelData';
begin
  Result := nil;
  try
    Result := FModelData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHModelManager.ModelName: string;
const OPNAME = 'TRWHModelManager.ModelName';
begin
  Result := '';
  try
    Result := CRWH;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRWHModelManager.ProcessCustomModelEvent (AData : TModelMenuData) : boolean;
const OPNAME = 'TRWHModelManager.ProcessCustomModelEvent';
begin
  Result := inherited ProcessCustomModelEvent(AData);
  try
    if Assigned(FAppModules.MainForm()) and
      Assigned(FAppModules.MainForm.PageControl) then
    begin
      {if ((Assigned(FGaugeSelectionTabSheetManager)) AND
          (Assigned(FGaugeSelectionTabSheetManager.TabSheet)) AND
          (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FGaugeSelectionTabSheetManager.TabSheet)) then
        Result := FGaugeSelectionTabSheetManager.DoCustomTabSheetEvent(AData);}
    end;
    if not Result then
      Result := inherited ProcessCustomModelEvent(AData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHModelManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TRWHModelManager.ProcessEvent';
begin
  Result := False;
  try
    Result := True;
    if(AEventType = CmeRWHUserGuide) then
      DoLaunchRWHUserGuide
      //CmeCreateIFRSite : DoCreateRWHSite('');
      //CmeDeleteIFRSite : DoDeleteRWHSite(NullInteger);
      //CmeSaveIFRSite   : DoSaveRWHSite(NullInteger);
    else
      Result := inherited ProcessEvent(AEventType, AData);
    //end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHModelManager.StudyDataHasChanged( AContext: TChangeContext;
                                                        AFieldName, AOldValue,
                                                        ANewValue: String ): Boolean;
const OPNAME = 'TRWHModelManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    {if (UpperCase(AFieldName) = 'CHANGELISTAPPLY') OR
       ((UpperCase(AFieldName) = 'CHANGELIST') AND (AContext = sdccDelete)) OR
       (UpperCase(AFieldName) = 'CHANGELISTCOPY')  OR
       (UpperCase(AFieldName) = 'ELEMENTACTIVE') OR
       (UpperCase(AFieldName) = 'ELEMENTORDER') then
    begin
      if Assigned(FModelData) then
        Result := Result AND FModelData.RefreshStationData;
    end;
    if (UpperCase(AFieldName) = 'USER GAUGES') then
    begin
      if Assigned(FModelData) then
        Result := Result AND FModelData.RefreshUserGauges;
    end;

    if Assigned(FGaugeSelectionTabSheetManager) then
      Result := Result AND FGaugeSelectionTabSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    }
    if Assigned(FGaugeSelectionTabSheetManager) then
      Result := Result AND FGaugeSelectionTabSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);

    if Assigned(FViewGaugeDataTabSheetManager) then
      Result := Result AND FViewGaugeDataTabSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);


    Result := True;
  except on E: Exception do HandleError( E, OPNAME ); end;
end;

function TRWHModelManager.ViewInputDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): Boolean;
const OPNAME = 'TRWHModelManager.ViewInputDialog';
//var
//  LDataList   : TStringList;
//  LIdentifier : integer;
begin
  Result := False;
  try
    {FMenuItemManager.SetCreateRWHSite(True);
    FMenuItemManager.SetDeleteRWHSite(False);
    FMenuItemManager.SetSaveRWHSite(False);

    Result := FModelGUIManager.ViewInputDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));

    if Assigned(FRWHTabSheetManager) and Assigned(FMenuItemManager) and
      (TTreeViewTabSheet(FRWHTabSheetManager.TabSheet).TreeView.Selected <> nil) and
      (TTreeViewTabSheet(FRWHTabSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      LDataList := TStringList.Create;
      try
        LDataList.CommaText := ACommaTextContextData;
        LIdentifier := StrToInt(LDataList.Values['MODELELEMENTID']);
        if(FModelData.RWHSiteDataList.RWHSiteDataByIdentifier[LIdentifier] <> nil) then
        begin
          FMenuItemManager.SetCreateRWHSite(True);
          FMenuItemManager.SetDeleteRWHSite(True);
          FMenuItemManager.SetSaveRWHSite(False);
        end;
      finally
        FreeAndNil(LDataList);
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{function TRWHModelManager.AddAndSelectElement (AViewID : string; AParentID : string; AWeighting : integer;
                                               ACaption : string; ABitmapName : string; ADataType: string;
                                               ASelect : boolean = True) : boolean;
const OPNAME = 'TRWHModelManager.AddAndSelectElement';
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
        LTreeView   := TTreeViewTabSheet(FRWHTabSheetManager.TabSheet).TreeView;
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
            if (ADataType = 'RWHSITEDATA') then
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
end;}

{function TRWHModelManager.DeleteElement(ATreeNode : TTreeNode) : boolean;
const OPNAME = 'TRWHModelManager.DeleteElement';
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

        LTreeView    := TTreeViewTabSheet(FRWHTabSheetManager.TabSheet).TreeView;
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
end;}

{function TRWHModelManager.DoCreateRWHSite(ACSVFileName: string): TRWHSiteDataObject;
const OPNAME = 'TRWHModelManager.DoCreateRWHSite';
var
  LRWHSiteData : TRWHSiteDataObject;
  LFileSelector: TOpenDialog;
  LLoadAgent:TRWHDataLoadAgent;
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

    LLoadAgent := TRWHDataLoadAgent.Create(FAppModules);
    try
      if not LLoadAgent.ValidateCSVFile(ACSVFileName) then
        Exit;
      LRWHSiteData := FModelData.RWHSiteDataList.CreateRWHSite(ACSVFileName);
      if LRWHSiteData <> nil then
      begin
        LLoadAgent.LoadDataFromFile(ACSVFileName,LRWHSiteData);
        AddAndSelectElement('RWHSITEDATA',
                            'RWHSITEDATA',
                            LRWHSiteData.SiteIdentifier,
                            LRWHSiteData.SiteName,
                            'SMALLCLOCK',
                            'RWHSITEDATA');
         StudyDataHasChanged(sdccAdd, 'RWHSite', '', IntToStr(LRWHSiteData.SiteIdentifier));
         if(FMenuItemManager <> nil) then
          FMenuItemManager.SetSaveRWHSite(True);
      end;
      Result := LRWHSiteData;
    finally
      LLoadAgent.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;}

{function TRWHModelManager.DoDeleteRWHSite(ARWHSiteID: integer): boolean;
const OPNAME = 'TRWHModelManager.DoDeleteRWHSite';
var
 LNode : TTreeNode;
begin
  Result := False;
  try
    if(ARWHSiteID = NullInteger) then
    begin
      if Assigned(FRWHTabSheetManager) then
      begin
        LNode := TTreeViewTabSheet(FRWHTabSheetManager.TabSheet).TreeView.Selected;
        if(LNode <> nil) and (LNode.Data <> nil) and (LNode.Level = 1) then
        begin
          ARWHSiteID := TViewDataTreeNodeData(LNode.Data).ViewDataNode.Weighting;
        end;
      end;
    end;

    Result := FModelData.RWHSiteDataList.DeleteRWHSite(ARWHSiteID);
    if Result then
    begin
      if Assigned(FRWHTabSheetManager) then
      begin
        FMenuItemManager.SetDeleteRWHSite(False);
        FMenuItemManager.SetSaveRWHSite(False);
        DeleteElement(TTreeViewTabSheet(FRWHTabSheetManager.TabSheet).TreeView.Selected);
      end;
      StudyDataHasChanged(sdccDelete, 'RWHSite', '', IntToStr(ARWHSiteID));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRWHModelManager.DoSaveRWHSite(ARWHSiteID: integer): boolean;
const OPNAME = 'TRWHModelManager.DoSaveRWHSite';
var
 LNode : TTreeNode;
begin
  Result := False;
  try
    if(ARWHSiteID = NullInteger) then
    begin
      if Assigned(FRWHTabSheetManager) then
      begin
        LNode := TTreeViewTabSheet(FRWHTabSheetManager.TabSheet).TreeView.Selected;
        if(LNode <> nil) and (LNode.Data <> nil) and (LNode.Level = 1) then
        begin
          ARWHSiteID := TViewDataTreeNodeData(LNode.Data).ViewDataNode.Weighting;
        end;
      end;
    end;

    Result := FModelData.RWHSiteDataList.SaveRWHSite(ARWHSiteID);

    if Result then
    begin
      if Assigned(FMenuItemManager) then
      begin
        FMenuItemManager.SetSaveRWHSite(False);
      end;
      StudyDataHasChanged(sdccSaveData, 'RWHSite', '', IntToStr(ARWHSiteID));
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;}

function TRWHModelManager.Validate(var AErrors: WideString; const AContext: WideString): WordBool;
const OPNAME = 'TRWHModelManager.Validate';
begin
  Result := False;
  try
    //Result := FModelData.Validate(AErrors,AContext);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRWHModelManager.DoLaunchRWHUserGuide;
const OPNAME = 'TRWHModelManager.DoLaunchRWHUserGuide';
var
  LFileName : string;
begin
  try
    if IsAcrobatReaderInstalled then
    begin
      LFileName := ExtractFilePath(ApplicationExeName);
      TPDFDocumentLauncher.Launch(LFileName + 'help\RRWH User Manual.pdf');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
