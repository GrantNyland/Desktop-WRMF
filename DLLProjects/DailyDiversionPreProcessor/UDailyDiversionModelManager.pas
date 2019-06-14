//
//
//  UNIT      : Contains TModelRainfallDataTabSheet Class
//  AUTHOR    : Sam Dhlamini (arivia.kom)
//  DATE      : 10/08/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UDailyDiversionModelManager;

interface

uses

  Contnrs,
  Classes,
  VCL.ComCtrls,
  UAbstractObject,
  UGenericModelLinkClasses,
  USystemModelManager,
  UTabSheetManager,
  UFilesActionDailyDiversionManager,
  UDailyDiversionDataObject,
  UDailyDiversionGUIManager,
  UDailyDiversionMenuItemManager,
  UDailyDiversionTabSheetManager,
  VoaimsCom_TLB;

type

  TDailyDiversionModelManager = class(TSystemModelManager)
  protected
    FModelData                             : TDailyDiversionDataObject;
    FDailyDiversionSheetManager            : TDailyDiversionTabSheetManager;
    FMenuItemManager                       : TDailyDiversionMenuItemManager;
    FModelGUIManager                       : TDailyDiversionGUIManager;
    FModelFilesActionManager               : TFilesActionDailyDiversionManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure RefreshMenuItems; override;
    procedure CreateFileActionManager;
    procedure DoLaunchDailyDiversionUserGuide;
    function DoImportDailyFlowDataFromCSVFile : boolean;
    function DoImportDailyInstreamFlowFile : boolean;
    function DoClearDailyInstreamFlowFile : boolean;
    function DoClearDailyFlowDataFromCSVFile : boolean;
    function DoClearFile14 : boolean;
    procedure DoCreateDailyDiversion;
    procedure DoRenameDailyDiversion;
    procedure DoDeleteDailyDiversion;
    function DoGenerateFlowDiversionRelation : boolean;
    function DoGenerateWRYMData : boolean;
    function DoClearFlowDiversionRelation : boolean;
    function DoImportFile14 : boolean;
    function DoExportDailyIFR : boolean;
    function DoExportMonthlyIFR : boolean;
    function DoExportFlowDiversionRelationship : boolean;
    function DeleteElement(ATreeNode : TTreeNode) : boolean;
    function AddAndSelectElement(AViewID : string; AParentID : string; AWeighting : integer;
                                 ACaption : string; ABitmapName : string; ADataType: string;
                                 ASelect : boolean = True) : boolean;
  public
    function ModelName: string; override;
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
    //function DoStationFilter(AChangeListID : integer) : WordBool;override;
    function DoExportSingleFile : boolean;
    function DoImportSingleFile: boolean;
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

    property MenuItemManager :  TDailyDiversionMenuItemManager read FMenuItemManager;

  end;

implementation

uses
  System.UITypes,
  VCL.Controls,
  SysUtils,
  VCL.Dialogs,
  UConstants,
  UViewDataItem,
  UAbstractComponent,
  UTreeViewTabSheet,
  UMainMenuEventType,
  UDailyDiversionGaugeData,
  URenameDailyDiversionForm,
  UIsAcrobatReaderInstalled,
  UPDFDocumentLauncher,
  UErrorHandlingOperations, Math;

procedure TDailyDiversionModelManager.CreateMemberObjects;
const OPNAME = 'TDailyDiversionModelManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FModelData := TDailyDiversionDataObject.Create(FAppModules);
    if Assigned(FAppModules.MainForm()) and
      Assigned(FAppModules.MainForm.PageControl) then
    begin
      FMenuItemManager := TDailyDiversionMenuItemManager.Create(FAppModules);
      FDailyDiversionSheetManager := TDailyDiversionTabSheetManager.Create(FAppModules);
      FDailyDiversionSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
      FModelGUIManager := TDailyDiversionGUIManager.Create(FAppModules);
      CreateFileActionManager;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionModelManager.DeleteModelDataViewer(AParent,
  AOwner: TObject): Boolean;
const OPNAME = 'TDailyDiversionModelManager.DeleteModelDataViewer';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDailyDiversionModelManager.DestroyMemberObjects;
const OPNAME = 'TDailyDiversionModelManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    if Assigned(FDailyDiversionSheetManager) then
      FreeAndNil(FDailyDiversionSheetManager);
    if Assigned(FMenuItemManager) then
      FreeAndNil(FMenuItemManager);
    if Assigned(FModelData) then
      FreeAndNil(FModelData);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionModelManager.DoExportSingleFile: boolean;
const OPNAME = 'TDailyDiversionModelManager.DoExportSingleFile';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoImportSingleFile: boolean;
const OPNAME = 'TDailyDiversionModelManager.DoImportSingleFile';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoRefreshFileHints: boolean;
const OPNAME = 'TDailyDiversionModelManager.DoRefreshFileHints';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.Initialise: boolean;
const OPNAME = 'TDailyDiversionModelManager.Initialise';
begin
  Result := inherited Initialise;
  try
    if Assigned(FModelData) then
      FModelData.Initialise;
    if Assigned(FDailyDiversionSheetManager) then
      Result := Result and FDailyDiversionSheetManager.Initialise;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionModelManager.LanguageHasChanged: Boolean;
const OPNAME = 'TDailyDiversionModelManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned (FDailyDiversionSheetManager) then
      Result := Result and FDailyDiversionSheetManager.LanguageHasChanged;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionModelManager.ModelData: TInterfacedObject;
const OPNAME = 'TDailyDiversionModelManager.ModelData';
begin
  Result := nil;
  try
    Result := FModelData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.ModelName: string;
const OPNAME = 'TDailyDiversionModelManager.ModelName';
begin
  Result := '';
  try
    Result := CDailyDiversion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionModelManager.OnTabHasChanged(ASender: TObject);
const OPNAME = 'TDailyDiversionModelManager.OnTabHasChanged';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionModelManager.PopulateDataList(
  AModelDataChangeType: TModelDataChangeType; AList: TList): Boolean;
const OPNAME = 'TDailyDiversionModelManager.PopulateDataList';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionModelManager.ProcessCustomModelEvent (AData : TModelMenuData) : boolean;
const OPNAME = 'TDailyDiversionModelManager.ProcessCustomModelEvent';
begin
  Result := False;
  try
    if Assigned(FAppModules.MainForm()) and
      Assigned(FAppModules.MainForm.PageControl) then
    begin
      if ((Assigned(FDailyDiversionSheetManager )) and
        (Assigned(FDailyDiversionSheetManager.TabSheet)) and
        (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FDailyDiversionSheetManager.TabSheet)) then
        Result := FDailyDiversionSheetManager.DoCustomTabSheetEvent(AData);
    end;
    if not Result then
      Result := inherited ProcessCustomModelEvent(AData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TDailyDiversionModelManager.ProcessEvent';
begin
  Result := False;
  try
    Result := True;
    case AEventType of
      CmeImportDailyInstreamFlowFile     : DoImportDailyInstreamFlowFile;
      CmeClearDailyInstreamFlowFile      : DoClearDailyInstreamFlowFile;
      CmeImportDailyFlowDataFromCSVFile  : DoImportDailyFlowDataFromCSVFile;
      CmeClearDailyFlowDataFromCSVFile   : DoClearDailyFlowDataFromCSVFile;
      CmeGenerateFlowDiversionRelation   : DoGenerateFlowDiversionRelation;
      CmeClearFlowDiversionRelation      : DoClearFlowDiversionRelation;
      CmeImportFile14                    : DoImportFile14;
      CmeClearFile14                     : DoClearFile14;
      CmeGenerateWRYMData                : DoGenerateWRYMData;
      CmeClearWRYMData                   : ;
      CmeExportDailyIFR                  : DoExportDailyIFR;
      CmeExportMonthlyIFR                : DoExportMonthlyIFR;
      CmeExportFlowDiversionRelationship : DoExportFlowDiversionRelationship;
      CmeCreateDailyDiversion            : DoCreateDailyDiversion;
      CmeRenameDailyDiversion            : DoRenameDailyDiversion;
      CmeDeleteDailyDiversion            : DoDeleteDailyDiversion;
      CmeDailyDiversionUserGuide         : DoLaunchDailyDiversionUserGuide;
    else
      Result := inherited ProcessEvent(AEventType, AData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.ReselectElement(ATreeNode : TTreeNode) : boolean;
const OPNAME = 'TDailyDiversionModelManager.ReselectElement';
var
  LTreeView     : TTreeview;
begin
  Result := False;
  try
    if (FAppModules.MainForm = nil)  then
      Result := True
    else
    begin
      LTreeView := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView;
      LTreeView.Selected := nil;
      LTreeView.Selected := ATreeNode;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDailyDiversionModelManager.RefreshMenuItems;
const OPNAME = 'TDailyDiversionModelManager.RefreshMenuItems';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.StudyDataHasChanged( AContext: TChangeContext;
                                                        AFieldName, AOldValue,
                                                        ANewValue: String ): Boolean;
const OPNAME = 'TDailyDiversionModelManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Assigned(FDailyDiversionSheetManager) then
      Result := Result and FDailyDiversionSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if ((AFieldName = 'InstreamAvgFlow') and (AContext = sdccAdd)) or (AFieldName = 'ImportIFR')  then
      (FMenuItemManager as TDailyDiversionMenuItemManager).TreeNodeHasChanged(AOldValue, StrToInt(ANewValue));
  except on E: Exception do HandleError( E, OPNAME ); end;
end;

function TDailyDiversionModelManager.StudyHasChanged: Boolean;
const OPNAME = 'TDailyDiversionModelManager.StudyHasChanged';
begin
  Result := False;
  try
    if Assigned(FModelData) then
    begin
      Result := FModelData.LoadData;

    end;
    Result := Result and inherited StudyHasChanged;
    if Assigned(FDailyDiversionSheetManager) then
      Result := Result and FDailyDiversionSheetManager.StudyHasChanged;
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.StudyHasChanged;
    if Assigned(FMenuItemManager) then
      Result := Result and FMenuItemManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionModelManager.ViewInputDialog(AParent: TObject;
  ACommaTextContextData: String; AOwner: TObject): Boolean;
const OPNAME = 'TDailyDiversionModelManager.ViewInputDialog';
var
  LDataList   : TStringList;
  LViewType   : string;
  LIdentifier : integer;
begin
  Result := False;
  try
    Result := FModelGUIManager.ViewInputDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));
    if (Assigned(FMenuItemManager)) then
    begin
      LDataList := TStringList.Create;
      try
        LDataList.CommaText := ACommaTextContextData;
        LViewType   := UpperCase(Trim(LDataList.Values['VIEWNAME']));
        LIdentifier := StrToInt(LDataList.Values['MODELELEMENTID']);
        (FMenuItemManager as TDailyDiversionMenuItemManager).TreeNodeHasChanged(LViewType, LIdentifier);
      finally
        FreeAndNil(LDataList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionModelManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TDailyDiversionModelManager.ProcessParameterChangeEvent';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionModelManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TDailyDiversionModelManager.ProcessMetaDataEvent';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionModelManager.DoCreateDailyDiversion;
const OPNAME = 'TDailyDiversionModelManager.DoCreateDailyDiversion';
var
  LNewStationNo: string;
  LDiversionGauge : TDiversionGauge;
begin
  try
   LNewStationNo := InputBox('Enter new station no(eg.C1H012)','Station Number','');
   if (LNewStationNo <> '') then
   begin
     LDiversionGauge := FModelData.DailyDiversionGaugeDataList.NewDiversionGauge(LNewStationNo);
     if LDiversionGauge <> nil then
     begin
       AddAndSelectElement('DAILYDIVERSIONSTATIONDATA',
                           'DAILYDIVERSIONSTATIONDATA',
                           LDiversionGauge.StationID,
                           LDiversionGauge.StationNo,
                          'SMALLCLOCK',
                          'DAILYDIVERSIONDATA');
        StudyDataHasChanged(sdccAdd, 'StationNo', '', LDiversionGauge.StationNo);
     end;
   end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDailyDiversionModelManager.DoRenameDailyDiversion;
const OPNAME = 'TDailyDiversionModelManager.DoRenameDailyDiversion';
var
  LNewStationNo: string;
  LOldStationNo: string;
  LfrmRenameDailyDiversion : TfrmRenameDailyDiversion;
begin
  try
    LfrmRenameDailyDiversion := TfrmRenameDailyDiversion.Create(nil,FAppModules);
    try
      if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
      begin
        LOldStationNo := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text;
        LfrmRenameDailyDiversion.edtTop.Text := LOldStationNo;
        if Assigned(LfrmRenameDailyDiversion) then
          LfrmRenameDailyDiversion.ShowModal;
        LNewStationNo :=  LfrmRenameDailyDiversion.edtBottom.Text;
        if LfrmRenameDailyDiversion.ModalResult = mrOk then
        begin
          if (LNewStationNo <> '') and (LOldStationNo <> '') then
          begin
            if FModelData.DailyDiversionGaugeDataList.RenameDiversionGauge(LNewStationNo,LOldStationNo) then
            begin
              if Assigned(FModelData) then
              begin
                {FModelData.Initialise;
                FModelData.LoadData;
                FModelData.LoadFile14IFRData;
                }
                FAppModules.Model.StudyDataHasChanged(sdccEdit,'OldStationNo',LOldStationNo,LNewStationNo);
              end;
            end;
          end;
        end;  
      end;
    finally
      LfrmRenameDailyDiversion.Free;
    end;  
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.AddAndSelectElement (AViewID : string; AParentID : string; AWeighting : integer;
                                                          ACaption : string; ABitmapName : string; ADataType: string;
                                                          ASelect : boolean = True) : boolean;
const OPNAME = 'TDailyDiversionModelManager.AddAndSelectElement';
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
        LTreeView   := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView;
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
            if (ADataType = 'DAILYDIVERSIONSTATIONDATA') then
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


procedure TDailyDiversionModelManager.DoDeleteDailyDiversion;
const OPNAME = 'TDailyDiversionModelManager.DoDeleteDailyDiversion';
var
  LMsg,
  LExistingStationNo : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      LExistingStationNo := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text;
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
      begin
        LMsg := Format('are you sure you want to DELETE station no [%s]?',[LExistingStationNo]);
        if MessageDlg(LMsg,mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          if FModelData.DailyDiversionGaugeDataList.RemoveDiversionGauge(LStationID) then
            DeleteElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoGenerateFlowDiversionRelation : boolean;
const OPNAME = 'TDailyDiversionModelManager.DoGenerateFlowDiversionRelation';
var
  //LStation : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      //LStation := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text;
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
        Result := FModelData.GenerateFlowDiversionRelation(LStationID);
      if Result then
        ReselectElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoClearFlowDiversionRelation : boolean;
const OPNAME = 'TDailyDiversionModelManager.DoClearFlowDiversionRelation';
var
  //LStation : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      //LStation := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text;
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
        Result := FModelData.ClearFlowDiversionRelation(LStationID);
      if Result then
        ReselectElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoGenerateWRYMData : boolean;
const OPNAME = 'TDailyDiversionModelManager.DoGenerateWRYMData';
var
  //LStation : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      //LStation := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text;
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
        Result := FModelData.GenerateWRYMData(LStationID);
      if Result then
        ReselectElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DeleteElement(ATreeNode : TTreeNode) : boolean;
const OPNAME = 'TDailyDiversionModelManager.DeleteElement';
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

        LTreeView    := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView;
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


procedure TDailyDiversionModelManager.CreateFileActionManager;
const OPNAME = 'TDailyDiversionModelManager.CreateFileActionManager';
begin
  try
    FModelFilesActionManager := TFilesActionDailyDiversionManager.Create(FAppModules);
    if Assigned(FModelFilesActionManager) then
    begin
      FOwnedAppObjects.Add(FModelFilesActionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDailyDiversionModelManager.DoImportDailyFlowDataFromCSVFile: boolean;
const OPNAME = 'TDailyDiversionModelManager.DoImportDailyFlowDataFromCSVFile';
var
  LFileName : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if PromptForFileName(LFileName,'*.csv','','Select Daily Data Flow','',False) then
    begin
      //TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
        Result := TFilesActionDailyDiversionManager(FModelFilesActionManager).ImportDailyDataFlowFile(LFileName,LStationID);
      if Result then
        ReselectElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);

        //RefreshModelData;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoImportDailyInstreamFlowFile : boolean;
const OPNAME = 'TDailyDiversionModelManager.DoImportDailyInstreamFlowFile';
var
  LFileName : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if PromptForFileName(LFileName,'*.csv','','Select Daily Instream Flow','',False) then
    begin
    
      //TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
        Result := TFilesActionDailyDiversionManager(FModelFilesActionManager).ImportDailyInstreamFlowFile(LFileName,LStationID);
      if Result then
        ReselectElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);

        //RefreshModelData;

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoImportFile14 : boolean;
const OPNAME = 'TDailyDiversionModelManager.DoImportFile14';
var
  LFileName : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if PromptForFileName(LFileName,'*.dat','','Select F014','',False) then
    begin
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;

      if LStationID <> NullInteger then
      Result := TFilesActionDailyDiversionManager(FModelFilesActionManager).ImportFile14(LFileName,LStationID);
      if Result then
        ReselectElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);
        //RefreshModelData;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TDailyDiversionModelManager.DoClearDailyInstreamFlowFile : boolean;
const OPNAME = 'TDailyDiversionModelManager.DoClearDailyInstreamFlowFile';
var
  //LStation : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      //LStation := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text;

      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
      begin
        Result := FModelData.ClearDailyInstreamFlowFileData(LStationID);
        Result := Result and FModelData.ClearFlowDiversionRelation(LStationID);
      end;  
      if Result then
        ReselectElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoClearDailyFlowDataFromCSVFile : boolean;
const OPNAME = 'TDailyDiversionModelManager.DoClearDailyFlowDataFromCSVFile';
var
  //LStation : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      //LStation := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text;
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
      begin
        Result := FModelData.ClearDailyFlowDataFromCSVFile(LStationID);
        Result := Result and FModelData.ClearFlowDiversionRelation(LStationID);
      end;
      if Result then
        ReselectElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoClearFile14 : boolean;
const OPNAME = 'TDailyDiversionModelManager.DoClearFile14';
var
  //LStation : string;
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      //LStation := TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Text;
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
      begin
        Result := FModelData.ClearFile14(LStationID);
        Result := Result and FModelData.ClearFlowDiversionRelation(LStationID);
      end;
      if Result then
        ReselectElement(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;

end;

function TDailyDiversionModelManager.RefreshModelData: boolean;
const OPNAME = 'TDailyDiversionModelManager.RefreshModelData';
begin
  Result :=False;
  try
    Initialise;
    StudyHasChanged;
    LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionModelManager.DoExportDailyIFR: boolean;
const OPNAME = 'TDailyDiversionModelManager.DoExportDailyIFR';
var
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
        Result := FModelData.ExportDailyIFR(LStationID);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoExportMonthlyIFR: boolean;
const OPNAME = 'TDailyDiversionModelManager.DoExportMonthlyIFR';
var
  LStationID : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
        Result := FModelData.ExportMonthlyIFR(LStationID);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.DoExportFlowDiversionRelationship: boolean;
const OPNAME = 'TDailyDiversionModelManager.DoExportFlowDiversionRelationship';
var
  LStationID  : integer;
  LDataObject : TViewDataTreeNodeData;
begin
  Result := False;
  try
    if (TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Level = 1) then
    begin
      LDataObject := TViewDataTreeNodeData(TTreeViewTabSheet(FDailyDiversionSheetManager.TabSheet).TreeView.Selected.Data);
      LStationID := NullInteger;
      if LDataObject <> nil then
        LStationID := LDataObject.ViewDataNode.Weighting;
      if LStationID <> NullInteger then
        Result := FModelData.ExportFlowDiversionRelationship(LStationID);
    end;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDailyDiversionModelManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TDailyDiversionModelManager.CanCopyToCLipboard';
begin
  Result := False;
  try
    if Assigned(FModelGUIManager) then
      Result := FModelGUIManager.CanCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionModelManager.CanExport: boolean;
const OPNAME = 'TDailyDiversionModelManager.CanExport';
begin
  Result := False;
  try
    if Assigned(FModelGUIManager) then
      Result := FModelGUIManager.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDailyDiversionModelManager.CanPrint: boolean;
const OPNAME = 'TDailyDiversionModelManager.CanPrint';
begin
  Result := False;
  try
    if Assigned(FModelGUIManager) then
      Result := FModelGUIManager.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionModelManager.DoCopyToCLipboard;
const OPNAME = 'TDailyDiversionModelManager.DoCopyToCLipboard';
begin
  try
    if Assigned(FModelGUIManager) then
      FModelGUIManager.DoCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionModelManager.DoExport(AFileName: string);
const OPNAME = 'TDailyDiversionModelManager.DoExport';
begin
  try
    if Assigned(FModelGUIManager) then
      FModelGUIManager.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionModelManager.DoPrint;
const OPNAME = 'TDailyDiversionModelManager.DoPrint';
begin
  try
    if Assigned(FModelGUIManager) then
      FModelGUIManager.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDailyDiversionModelManager.DoLaunchDailyDiversionUserGuide;
const OPNAME = 'TDailyDiversionModelManager.DoLaunchDailyDiversionUserGuide';
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
