//
//
//  UNIT      : Contains TDDTSModelManager Class
//  AUTHOR    : Sam Dhlamini (bcx)
//  DATE      : 07/04/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UDDTSModelManager;

interface

uses

  Contnrs,
  Classes,
  VCL.ComCtrls,
  UAbstractObject,

  UGenericModelLinkClasses,
  USystemModelManager,
  UFileselectionManager,
  UTabSheetManager,
  UFilesActionDDTSManager,
  UDDTSGUIManager,
  UDDTSDataObject,
  UDDTSMenuItemManager,
  UDDTSTabSheetManager,

  UDataViewerManager,
  UDataFileObjects,
  UAbstractFileNamesObject,
  UFilesActionAbstractManager,
  UWeatherEventsTabSheetManager,

  VoaimsCom_TLB;

type

  TDDTSModelManager = class(TSystemModelManager)
  protected
    FLoadingData                           : boolean;
    FOldShortDateFormat                    : string;
    FModelData                             : TDDTSDataObject;
    FDDTSSheetManager                      : TDDTSTabSheetManager;
    FModelMenuItemManager                  : TDDTSMenuItemManager;
    FModelGUIManager                       : TDDTSGUIManager;
    FModelFilesActionManager               : TFilesActionDDTSManager;
    FFileEditManager                       : TAbstractFileEditManager;
    FFileSelectionManager                  : TFileselectionManager;
    FOutputReviewManager                   : TTabSheetManager;


    FWeatherEventsTabSheetManager          : TWeatherEventsTabSheetManager;
    FModelCapabilityManager                : TTabSheetManager;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure RefreshMenuItems; override;
    procedure CreateFileEditManager; virtual;
    procedure CreateOutputReviewManager;
    procedure CreateFileActionManager;
    procedure CreateFileSelectionManager; virtual;
    procedure CreateModelCapabilityManager; virtual;

    function LoadModelData: boolean; virtual;
    function SelectModelFileNames: boolean; virtual;
    function DisplayModelFileNames: boolean; virtual;
    function UpdateOutputData: boolean; virtual;
    procedure DoLaunchDailyDiversionUserGuide;
    procedure DoCreateDDTSDam;
    procedure SetSystemMenuState; virtual;
    function DeleteElement(ATreeNode : TTreeNode) : boolean;
    function AddAndSelectElement(AViewID : string; AParentID : string; AWeighting : integer;
                                 ACaption : string; ABitmapName : string; ADataType: string;
                                 ASelect : boolean = True) : boolean;
  public
    function ModelName: string; override;
    function Initialise: boolean; override;
    function PopulateDataList(AModelDataChangeType: TModelDataChangeType; AList: TList): Boolean; override;
    function ViewInputDialog(AParent: TObject; ACommaTextContextData: String;AOwner: TObject = nil): Boolean; override;
    function ViewOutputDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean; override;
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

    function DoValidateSingleFile: boolean;
    function DoValidateAllFiles: WordBool;
    function DoExportAllFiles: WordBool;
    function DoExportSingleFile: boolean;
    function DoImportAllFiles: WordBool;
    function DoImportSingleFile: boolean;
    function DoClearModelData: WordBool;
    function DoValidateModelData: boolean;
    function DoRunModel: WordBool;
    function DoExportAllFilesAndRunModel(ASilent: WordBool): WordBool;

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

    property MenuItemManager :  TDDTSMenuItemManager read FModelMenuItemManager;

  end;

implementation

uses
  VCL.Controls,
  System.SysUtils,
  VCL.Dialogs,

  UDDTSData,
  UUtilities,
  UFileNames,
  UConstants,
  UViewDataItem,
  UAbstractComponent,
  UTreeViewTabSheet,
  UPDFDocumentLauncher,
  UMainMenuEventType,
  UDDTSOutputCSVFileAgent,
  UDDTSFileselectionManager,
  UIsAcrobatReaderInstalled,
  UErrorHandlingOperations, Math;

procedure TDDTSModelManager.CreateMemberObjects;
const OPNAME = 'TDDTSModelManager.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FOldShortDateFormat            := FormatSettings.ShortDateFormat;
    FormatSettings.ShortDateFormat := 'yyyy/mm/dd';
    FModelData := TDDTSDataObject.Create(FAppModules);
    if Assigned(FAppModules.MainForm()) and
      Assigned(FAppModules.MainForm.PageControl) then
    begin
      FModelGUIManager := TDDTSGUIManager.Create(FAppModules);
      FModelMenuItemManager := TDDTSMenuItemManager.Create(FAppModules);
      FDDTSSheetManager := TDDTSTabSheetManager.Create(FAppModules);
      FDDTSSheetManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;

      CreateFileSelectionManager;
      CreateFileEditManager;
      CreateFileActionManager;
      CreateOutputReviewManager;
      FWeatherEventsTabSheetManager                         := TWeatherEventsTabSheetManager.Create(FAppModules);
      FWeatherEventsTabSheetManager.TabSheet.PageControl    := FAppModules.MainForm.PageControl;
      CreateModelCapabilityManager;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSModelManager.DestroyMemberObjects;
const OPNAME = 'TDDTSModelManager.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FormatSettings.ShortDateFormat := FOldShortDateFormat;
    if Assigned(FDDTSSheetManager) then
      FreeAndNil(FDDTSSheetManager);
    if Assigned(FModelMenuItemManager) then
      FreeAndNil(FModelMenuItemManager);
    if Assigned(FModelData) then
      FreeAndNil(FModelData);
    if Assigned(FWeatherEventsTabSheetManager) then
      FreeAndNil(FWeatherEventsTabSheetManager);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSModelManager.Initialise: boolean;
const OPNAME = 'TDDTSModelManager.Initialise';
begin
  Result := inherited Initialise;
  try
    if Assigned(FModelData) then
      FModelData.Initialise;
    if Assigned(FDDTSSheetManager) then
      Result := Result and FDDTSSheetManager.Initialise;
    if Assigned(FModelMenuItemManager) then
      Result := Result and FModelMenuItemManager.Initialise;
    if Assigned(FWeatherEventsTabSheetManager) then
      Result := Result and FWeatherEventsTabSheetManager.Initialise;
    if Assigned(FFileEditManager) then
      Result := Result and FFileEditManager.Initialise;
    if Assigned(FModelCapabilityManager) then
      FModelCapabilityManager.Initialise;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSModelManager.LanguageHasChanged: Boolean;
const OPNAME = 'TDDTSModelManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if Assigned (FDDTSSheetManager) then
      Result := Result and FDDTSSheetManager.LanguageHasChanged;
    if Assigned(FModelMenuItemManager) then
      Result := Result and FModelMenuItemManager.LanguageHasChanged;
    if Assigned (FWeatherEventsTabSheetManager) then
      FWeatherEventsTabSheetManager.LanguageHasChanged;
    if Assigned (FFileEditManager) then
      FFileEditManager.LanguageHasChanged;
    if Result and Assigned(FFileSelectionManager) then
        Result := FFileSelectionManager.LanguageHasChanged;
    if Result and Assigned(FModelCapabilityManager) then
        Result := FModelCapabilityManager.LanguageHasChanged;
    if Result and Assigned(FOutputReviewManager) then
        Result := FOutputReviewManager.LanguageHasChanged;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSModelManager.ModelData: TInterfacedObject;
const OPNAME = 'TDDTSModelManager.ModelData';
begin
  Result := nil;
  try
    Result := FModelData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSModelManager.ModelName: string;
const OPNAME = 'TDDTSModelManager.ModelName';
begin
  Result := '';
  try
    Result := CDDTS;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DeleteModelDataViewer(AParent,
  AOwner: TObject): Boolean;
const OPNAME = 'TDDTSModelManager.DeleteModelDataViewer';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSModelManager.DoRefreshFileHints: boolean;
const OPNAME = 'TDDTSModelManager.DoRefreshFileHints';
begin
  Result := False;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDDTSModelManager.OnTabHasChanged(ASender: TObject);
const OPNAME = 'TDDTSModelManager.OnTabHasChanged';
begin
  inherited;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSModelManager.PopulateDataList(AModelDataChangeType: TModelDataChangeType; AList: TList): Boolean;
const OPNAME = 'TDDTSModelManager.PopulateDataList';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSModelManager.ProcessCustomModelEvent (AData : TModelMenuData) : boolean;
const OPNAME = 'TDDTSModelManager.ProcessCustomModelEvent';
begin
  Result := False;
  try
    if Assigned(FAppModules.MainForm()) and
      Assigned(FAppModules.MainForm.PageControl) then
    begin
      if ((Assigned(FDDTSSheetManager )) and
        (Assigned(FDDTSSheetManager.TabSheet)) and
        (TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage) = FDDTSSheetManager.TabSheet)) then
        Result := FDDTSSheetManager.DoCustomTabSheetEvent(AData);
    end;
    if (not Result) and  Assigned(FModelCapabilityManager) then
      Result :=  FModelCapabilityManager.DoCustomTabSheetEvent(AData);

    if (not Result) and  Assigned(FOutputReviewManager) then
      Result := FOutputReviewManager.DoCustomTabSheetEvent(AData);

    if not Result then
      Result := inherited ProcessCustomModelEvent(AData);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSModelManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TDDTSModelManager.ProcessEvent';
begin
  Result := False;
  try
    Result := True;
    case AEventType of
      CmeValidateFile                 : DoValidateSingleFile;
      CmeImportFile                   : DoImportSingleFile;
      CmeExportFile                   : DoExportSingleFile;
      CmeValidateFiles                : DoValidateAllFiles;
      CmeImportFiles                  : DoImportAllFiles;
      CmeExportFiles                  : DoExportAllFiles;
      CmeClearModelData               : DoClearModelData;
      CmeRunModel                     : DoRunModel;
      CmeValidateModelData            : DoValidateModelData;
      CmeRefreshFileHints             : DoNothing;
    else
      Result := inherited ProcessEvent(AEventType, AData);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSModelManager.ReselectElement(ATreeNode : TTreeNode) : boolean;
const OPNAME = 'TDDTSModelManager.ReselectElement';
var
  LTreeView     : TTreeview;
begin
  Result := False;
  try
    if (FAppModules.MainForm = nil)  then
      Result := True
    else
    begin
      LTreeView := TTreeViewTabSheet(FDDTSSheetManager.TabSheet).TreeView;
      LTreeView.Selected := nil;
      LTreeView.Selected := ATreeNode;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSModelManager.RefreshMenuItems;
const OPNAME = 'TDDTSModelManager.RefreshMenuItems';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSModelManager.StudyDataHasChanged( AContext: TChangeContext;
                                                        AFieldName, AOldValue,
                                                        ANewValue: String ): Boolean;
const OPNAME = 'TDDTSModelManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Assigned(FDDTSSheetManager) then
      Result := Result and FDDTSSheetManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
    if Assigned(FFileSelectionManager) then
      Result := Result and FFileSelectionManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if Result and Assigned(FFileEditManager) then
        Result := FFileEditManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if (Assigned(FModelCapabilityManager)) then
      Result := Result AND FModelCapabilityManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
   if Result and Assigned(FOutputReviewManager) then
      Result := FOutputReviewManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);

  except on E: Exception do HandleError( E, OPNAME ); end;
end;

function TDDTSModelManager.StudyHasChanged: Boolean;
const OPNAME = 'TDDTSModelManager.StudyHasChanged';
begin
  Result := False;
  try
    Result := LoadModelData;
    Result := Result and inherited StudyHasChanged;
    if Assigned(FDDTSSheetManager) then
      Result := Result and FDDTSSheetManager.StudyHasChanged;
    if Assigned(FModelGUIManager) then
      Result := Result and FModelGUIManager.StudyHasChanged;
    if Assigned(FModelMenuItemManager) then
      Result := Result and FModelMenuItemManager.StudyHasChanged;
    if Assigned (FWeatherEventsTabSheetManager) then
      FWeatherEventsTabSheetManager.StudyHasChanged;
    if Assigned (FFileEditManager) then
      FFileEditManager.StudyHasChanged;
    if Result and Assigned(FModelCapabilityManager) then
      Result := Result and FModelCapabilityManager.StudyHasChanged;

    if Result  and Assigned(FOutputReviewManager)then
       Result := FOutputReviewManager.StudyHasChanged;

    if Assigned(FAppModules.MainForm()) then
    begin
      FAppModules.MainForm.ApplyPreviousTabIndex;
      OnTabHasChanged(nil);
      SetSystemMenuState;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSModelManager.ViewInputDialog(AParent: TObject;
  ACommaTextContextData: String; AOwner: TObject): Boolean;
const OPNAME = 'TDDTSModelManager.ViewInputDialog';
var
  LDataList   : TStringList;
  LViewType   : string;
  LIdentifier : integer;
begin
  Result := False;
  try
    Result := FModelGUIManager.ViewInputDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));
    if (Assigned(FModelMenuItemManager)) then
    begin
      LDataList := TStringList.Create;
      try
        LDataList.CommaText := ACommaTextContextData;
        LViewType   := UpperCase(Trim(LDataList.Values['VIEWNAME']));
        LIdentifier := StrToInt(LDataList.Values['MODELELEMENTID']);
        (FModelMenuItemManager as TDDTSMenuItemManager).TreeNodeHasChanged(LViewType, LIdentifier);
      finally
        FreeAndNil(LDataList);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


function TDDTSModelManager.ViewOutputDialog(AParent: TObject; ACommaTextContextData: String; AOwner: TObject): boolean;
const OPNAME = 'TYieldModelManager.TDDTSModelManager';
begin
  Result := FALSE;
  try
    Result := FModelGUIManager.ViewOutputDialog(TWincontrol(AParent), ACommaTextContextData, TWincontrol(AOwner));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TDDTSModelManager.ProcessParameterChangeEvent';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TDDTSModelManager.ProcessMetaDataEvent';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSModelManager.DoCreateDDTSDam;
const OPNAME = 'TDDTSModelManager.DoCreateDailyDiversion';
var
  LNewDam: string;
  LDDTSDetailData : TDDTSDetailData;
begin
  try
   LNewDam := InputBox('Enter new Dam','Dam Name','');
   if (LNewDam <> '') then
   begin
     LDDTSDetailData := FModelData.DDTSDamDataList.CreateDDTSDetailData(LNewDam);
     if LDDTSDetailData <> nil then
     begin
       AddAndSelectElement('DDTSDAMDATA',
                           'DDTSDAMDATA',
                           LDDTSDetailData.Identifier,
                           IntToStr(LDDTSDetailData.Identifier),
                          'RESERVOIR',
                          'DDTSDAMDATA');
        StudyDataHasChanged(sdccAdd, 'Identifier', '', IntToStr(LDDTSDetailData.Identifier));
     end;
   end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TDDTSModelManager.AddAndSelectElement (AViewID : string; AParentID : string; AWeighting : integer;
                                                          ACaption : string; ABitmapName : string; ADataType: string;
                                                          ASelect : boolean = True) : boolean;
const OPNAME = 'TDDTSModelManager.AddAndSelectElement';
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
        LTreeView   := TTreeViewTabSheet(FDDTSSheetManager.TabSheet).TreeView;
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
            if (ADataType = 'DDTSDAMDATA') then
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

function TDDTSModelManager.DeleteElement(ATreeNode : TTreeNode) : boolean;
const OPNAME = 'TDDTSModelManager.DeleteElement';
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

        LTreeView    := TTreeViewTabSheet(FDDTSSheetManager.TabSheet).TreeView;
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


procedure TDDTSModelManager.CreateFileActionManager;
const OPNAME = 'TDDTSModelManager.CreateFileActionManager';
begin
  try
    FModelFilesActionManager := TFilesActionDDTSManager.Create(FAppModules);
    if Assigned(FModelFilesActionManager) then
    begin
      FOwnedAppObjects.Add(FModelFilesActionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSModelManager.RefreshModelData: boolean;
const OPNAME = 'TDDTSModelManager.RefreshModelData';
begin
  Result :=False;
  try
    Initialise;
    StudyHasChanged;
    LanguageHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.CanCopyToCLipboard: boolean;
const OPNAME = 'TDDTSModelManager.CanCopyToCLipboard';
begin
  Result := False;
  try
    if Assigned(FModelGUIManager) then
      Result := FModelGUIManager.CanCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.CanExport: boolean;
const OPNAME = 'TDDTSModelManager.CanExport';
begin
  Result := False;
  try
    if Assigned(FModelGUIManager) then
      Result := FModelGUIManager.CanExport;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.CanPrint: boolean;
const OPNAME = 'TDDTSModelManager.CanPrint';
begin
  Result := False;
  try
    if Assigned(FModelGUIManager) then
      Result := FModelGUIManager.CanPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSModelManager.DoCopyToCLipboard;
const OPNAME = 'TDDTSModelManager.DoCopyToCLipboard';
begin
  try
    if Assigned(FModelGUIManager) then
      FModelGUIManager.DoCopyToCLipboard;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSModelManager.DoExport(AFileName: string);
const OPNAME = 'TDDTSModelManager.DoExport';
begin
  try
    if Assigned(FModelGUIManager) then
      FModelGUIManager.DoExport(AFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSModelManager.DoPrint;
const OPNAME = 'TDDTSModelManager.DoPrint';
begin
  try
    if Assigned(FModelGUIManager) then
      FModelGUIManager.DoPrint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSModelManager.DoLaunchDailyDiversionUserGuide;
const OPNAME = 'TDDTSModelManager.DoLaunchDailyDiversionUserGuide';
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

procedure TDDTSModelManager.CreateFileEditManager;
const OPNAME = 'TDDTSModelManager.CreateFileEditManager';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\FileEditor.dll',
      TAbstractAppObject(FFileEditManager), FAppModules, False, OPNAME);
    if Assigned(FFileEditManager) then
    begin
      FOwnedAppObjects.Add(FFileEditManager);
      FFileEditManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSModelManager.CreateOutputReviewManager;
const OPNAME = 'TDDTSModelManager.CreateOutputReviewManager';
begin
  if not Assigned(FAppModules.MainForm()) then Exit;
  try
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\OutputReview.dll',
      TAbstractAppObject(FOutputReviewManager), FAppModules, False, OPNAME);
    if Assigned(FOutputReviewManager) then
    begin
      FOwnedAppObjects.Add(FOutputReviewManager);
      FOutputReviewManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


procedure TDDTSModelManager.CreateFileSelectionManager;
const OPNAME = 'TDDTSModelManager.CreateFileSelectionManager';
begin
  try
    FFileSelectionManager := TDDTSFileSelectionManager.Create(FAppModules);
    if Assigned(FFileSelectionManager) then
    begin
      FOwnedAppObjects.Add(FFileSelectionManager);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSModelManager.CreateModelCapabilityManager;
const OPNAME = 'TDDTSModelManager.CreateModelCapabilityManager';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    CreateDLLObject(ExtractFilePath(ApplicationExeName) + 'bin\ModelCapability.dll',
      TAbstractAppObject(FModelCapabilityManager), FAppModules, False, OPNAME);
    if Assigned(FModelCapabilityManager) then
    begin
      FOwnedAppObjects.Add(FModelCapabilityManager);
      FModelCapabilityManager.TabSheet.PageControl := FAppModules.MainForm.PageControl;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



function TDDTSModelManager.SelectModelFileNames: boolean;
const OPNAME = 'TDDTSModelManager.SelectModelFileNames';
var
  LDataFileObjects: TDataFileObjects;
begin
  Result := False;
  try
    if not Assigned(FFileSelectionManager) then
    begin
      Result := True;
    end
    else
    begin
      LDataFileObjects := TDataFileObjects.Create;
      try
        Result := FFileSelectionManager.PopulateFileNames(LDataFileObjects,FModelData.CastFileNamesObject);
      finally
        FreeAndNil(LDataFileObjects);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DisplayModelFileNames: boolean;
const OPNAME = 'TDDTSModelManager.DisplayModelFileNames';
begin
  Result := False;
  try
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
       Result := FFileSelectionManager.PopulateTreeView(TTreeViewTabSheet(FFileEditManager.TabSheet).TreeView,
                 FModelData.CastFileNamesObject);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.LoadModelData: boolean;
const OPNAME = 'TDDTSModelManager.LoadModelData';
begin
  Result := False;
  try
    FLoadingData := True;
    try
      Result := SelectModelFileNames;
      Result := Result and FModelData.LoadModelData;
      Result := Result and DisplayModelFileNames;
      Result := Result and UpdateOutputData;
    finally
      FLoadingData := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.UpdateOutputData: boolean;
const OPNAME = 'TDDTSModelManager.UpdateOutputData';
var
  LFileAgent : TDDTSOutputCSVFileAgent;
  LAModelFileName : TAbstractModelFileName;
begin
  Result := False;
  try
    LAModelFileName := FModelData.CastFileNamesObject.OutputFileNames.FileNameObject[0];
    if FileExists(LAModelFileName.FileName) then
    begin
      LFileAgent := TDDTSOutputCSVFileAgent.Create(FAppModules);
      try
        LFileAgent.ReadModelDataFromFile(LAModelFileName,nil,nil);
      finally
        LFileAgent.Free;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;//dsr

procedure TDDTSModelManager.SetSystemMenuState;
const OPNAME = 'TDDTSModelManager.SetSystemMenuState';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    if (FModelData.CastFileNamesObject.FilesCount = 0) then
    begin
      TDDTSMenuItemManager(FModelMenuItemManager).SetMenuValidateFiles(msDisable);
      TDDTSMenuItemManager(FModelMenuItemManager).SetMenuImportFiles(msDisable);
      TDDTSMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msDisable);
      TDDTSMenuItemManager(FModelMenuItemManager).SetMenuClearModelData(msDisable);
      TDDTSMenuItemManager(FModelMenuItemManager).SetMenuRunModel(msDisable);
    end
    else
    begin
      TDDTSMenuItemManager(FModelMenuItemManager).SetMenuValidateModelData(msEnable);
      TDDTSMenuItemManager(FModelMenuItemManager).SetMenuImportFiles(msEnable);
      TDDTSMenuItemManager(FModelMenuItemManager).SetMenuRunModel(msEnable);
      if (FModelData.CastFileNamesObject.FilesSavedInDatabaseCount = 0) then
      begin
        TDDTSMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msDisable);
        TDDTSMenuItemManager(FModelMenuItemManager).SetMenuClearModelData(msDisable);
      end
      else
      begin
        TDDTSMenuItemManager(FModelMenuItemManager).SetMenuExportFiles(msEnable);
        TDDTSMenuItemManager(FModelMenuItemManager).SetMenuClearModelData(msEnable);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoValidateSingleFile: boolean;
const OPNAME = 'TDDTSModelManager.DoValidateSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoValidateSingleFile;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoValidateAllFiles: WordBool;
const OPNAME = 'TDDTSModelManager.DoValidateAllFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoValidateAllFiles;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoExportAllFiles: WordBool;
const OPNAME = 'TDDTSModelManager.DoExportAllFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoExportAllFiles;
    // Set System menu state.
    SetSystemMenuState;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.StudyDataHasChanged(sdccExport,'FileNames','','');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoImportAllFiles: WordBool;
const OPNAME = 'TDDTSModelManager.DoImportAllFiles';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoImportAllFiles;
    RefreshModelData

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoExportSingleFile: boolean;
const OPNAME = 'TDDTSModelManager.DoExportSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoExportSingleFile;
    // Set System menu state.
    SetSystemMenuState;
    if Assigned(FFileEditManager) and Assigned(FFileEditManager.TabSheet) then
      FFileEditManager.TabSheet.StudyDataHasChanged(sdccExport,'SingleFile','','');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoImportSingleFile: boolean;
const OPNAME = 'TDDTSModelManager.DoImportSingleFile';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoImportSingleFile;
    RefreshModelData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoClearModelData: WordBool;
const OPNAME = 'TDDTSModelManager.DoClearModelData';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoClearModelData;
    RefreshModelData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoExportAllFilesAndRunModel(ASilent: WordBool): WordBool;
const OPNAME = 'TDDTSModelManager.DoExportAllFilesAndRunModel';
begin
  Result := False;
  try
    if Asilent then
    begin
      FAppModules.GlobalData.SetStopOnFirstErr(False);
    end;

    Result := FModelFilesActionManager.DoExportAllFilesAndRunModel(ASilent);
    RefreshModelData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoRunModel: WordBool;
const OPNAME = 'TDDTSModelManager.DoRunModel';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoRunModel;
    if Result then
      RefreshModelData
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSModelManager.DoValidateModelData: boolean;
const OPNAME = 'TDDTSModelManager.DoValidateModelData';
begin
  Result := False;
  try
    Result := FModelFilesActionManager.DoValidateModelData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
