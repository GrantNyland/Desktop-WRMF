//
//
//  UNIT      : Contains TSystemModelManager Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/12/2001
//  COPYRIGHT : Copyright © 2001 DWAF
//
//
unit USystemModelManager;

interface

uses
  Classes,
  Contnrs,
  VCL.ComCtrls,
  UViewDataManager,
  UGenericModelLinkClasses,
  UAbstractComponent,
  UAbstractObject;

type
  TTreeViewSheetName   = (tvsnAll,tvsnData,tvsnOutput);
  TSystemModelManager = class(TAbstractModelManager)
  protected
    FCustomFieldEditArray : array of TCustomFieldEditArray;
    FLoadViewData         : boolean;
    FViewDataManager      : TViewDataManager;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function GetFieldUpdateFunction(AFieldName: String) : TFieldUpdateFunction; override;
    function GetFieldEditFunction(AFieldName: String) : TFieldEditFunction; override;
    procedure RefreshMenuItems; virtual;
    procedure DoOnHint(ASender: TObject); virtual;
    function ProcessCustomModelEvent(AData: TModelMenuData): boolean; virtual;
    function ProcessParameterChangeEvent : boolean; virtual;
    function ProcessMetaDataEvent : boolean; virtual;
  public
    function ModelName: string; override;
    function PopulateDataList(AModelDataChangeType : TModelDataChangeType; AList : TList) : boolean; override;
    function DeleteModelDataViewer(AParent : TObject; AOwner : TObject) : boolean; override;
    function CreateTabSheetTreeViewElement(AViewID,AParentIDsCommatext   : string;
                                        AWeighting,
                                        AParentWeighting  : integer;
                                        ACaption    : string;
                                        ABitmapName : string;
                                        ADataType   : string;
                                        ASelect     : boolean;
                                        ATreeViewSheetName:TTreeViewSheetName) : boolean; virtual;
    function DeleteTabSheetTreeViewElement (AViewID, AParentIDsCommatext : string;
                                            AWeighting,AParentWeighting  : integer) : boolean; virtual;

    function ViewData: TAbstractViewDataList; override;
    function Initialise: boolean; override;
    function ModelData: TInterfacedObject; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    function ModelDataLoaded: boolean; override;
    function ProcessEvent(AEventType: integer; AData: TObject): boolean; override;
    procedure OnTabChangeRequest(ASender: TObject; var AAllowChange: Boolean); virtual;
    procedure OnTabHasChanged(ASender: TObject); virtual;
    function EditFieldValue(AFieldName, AOldValue : string; AContextData: TStrings): boolean; override;
    //function EditFieldValue(AFieldName, AOldValue, ADefaultValue: string; AContextData: TStrings): boolean; override;
    function DoesFieldHaveACustomEditor(AFieldName: String): Boolean; override;
    procedure DoModelDataHasChanged(AChangeLevel:TModelDataChangeLevel;AChangeType:TModelDataChangeType;
              AChangeAction:TModelDataChangeAction); override;
    function ValidateBusinessRule(ABusinessRule: integer; AObjectsArray: array of TObject;
             AErrorMessages: TStrings;AOptionalValues: string=''): boolean; override;
    //function DoStationFilter(AChangeListID : integer) : WordBool;override;
  end;

implementation

uses

  // Delphi
  SysUtils,
  Windows,
  VCL.Controls,
  VCL.ExtCtrls,
  VCL.Forms,
  Messages,

  // DWAF
  UStudyArea,
  UDataSetType,
  UDLLOperations,
  USQLDatabaseLayer,
  UMainMenuEventType,
  USystemModelLinkClasses,
  USystemModelDataSetConstructor,
//  UFieldPropertyEditorForm,
  UErrorHandlingOperations;

procedure TSystemModelManager.CreateMemberObjects;
const OPNAME = 'TSystemModelManager.CreateMemberObjects';
begin
  try
    FLoadViewData := True;

    // Call the ancestor.
    inherited CreateMemberObjects;

    // Create member objects.
    if Assigned(FAppModules.MainForm()) then
    begin
      FViewDataManager := TViewDataManager.Create(FAppModules);

      // Add all owned app objects.
      FOwnedAppObjects.Add(FViewDataManager);
    end;
    // Add the data set constructor.
    TSQLDatabaseLayer(FAppModules.Database).AddDataSetConstructor(TSystemModelDatasetConstructor.Create(FAppModules));

    // Set application event handlers.
    Application.OnHint := DoOnHint;
    Application.ShowHint := True;
    if Assigned(FAppModules.MainForm()) and Assigned(FAppModules.MainForm.PageControl) then
      FAppModules.MainForm.PageControl.ShowHint := True;
    DoOnHint(nil);
    SetLength(FCustomFieldEditArray,0);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSystemModelManager.DestroyMemberObjects;
const OPNAME = 'TSystemModelManager.DestroyMemberObjects';
begin
  try
    if Assigned(FAppModules.MainForm()) then
    begin
      FAppModules.MainForm.ChildToolBar := nil;
      FAppModules.MainForm.PageControl.ActivePageIndex := -1;
    end;
    TSQLDatabaseLayer(FAppModules.Database).DeleteDataSetConstructorsOfType(TSystemModelDatasetConstructor);
    SetLength(FCustomFieldEditArray,0);
    FCustomFieldEditArray := nil;
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.Initialise: boolean;
const OPNAME = 'TSystemModelManager.Initialise';
begin
  Result := False;
  try
    inherited Initialise;

    // Set the tab change event handlers.
    if Assigned(FAppModules.MainForm()) then
    begin
      FAppModules.MainForm.PageControl.OnChanging := OnTabChangeRequest;
      FAppModules.MainForm.PageControl.OnChange   := OnTabHasChanged;

      FAppModules.MainForm.MenuItemManager.Show;
    end;

    if Assigned(FAppModules.StudyDocumentManager()) then
      FAppModules.StudyDocumentManager.Initialise;
    Result := true;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSystemModelManager.ModelData: TInterfacedObject;
const OPNAME = 'TSystemModelManager.ModelData';
begin
  Result := nil;
end;

function TSystemModelManager.ViewData: TAbstractViewDataList;
const OPNAME = 'TSystemModelManager.ViewData';
begin
  Result := nil;
  try
    if Assigned(FViewDataManager) and Assigned(FViewDataManager.ViewDataList) then
      Result := FViewDataManager.ViewDataList;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSystemModelManager.RefreshMenuItems;
const OPNAME = 'TSystemModelManager.RefreshMenuItems';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;

    FAppModules.MainForm.MenuItemManager.SetViewToolBarChecked(
      FAppModules.MainForm.MenuItemManager.IsToolBarVisible);
    FAppModules.MainForm.MenuItemManager.SetViewStudyPanelChecked(
      FAppModules.MainForm.StudyPanelVisible);
    if Assigned(FAppModules.MainForm.ActivePage) then
    begin
      FAppModules.MainForm.MenuItemManager.SetClipboardEnabled(
        TAbstractTabSheet(FAppModules.MainForm.ActivePage).CanCopyToCLipboard);
      FAppModules.MainForm.MenuItemManager.SetExportEnabled(
        TAbstractTabSheet(FAppModules.MainForm.ActivePage).CanExport);
      if Assigned(FAppModules.PrintManager()) then
        FAppModules.PrintManager.SetPrintEnabled(
          TAbstractTabSheet(FAppModules.MainForm.ActivePage).CanPrint);
    end
    else
    begin
      FAppModules.MainForm.MenuItemManager.SetClipboardEnabled(False);
      FAppModules.MainForm.MenuItemManager.SetExportEnabled(False);
      if Assigned(FAppModules.PrintManager()) then
        FAppModules.PrintManager.SetPrintEnabled(False);
    end;
    if Assigned(FViewDataManager) then
      FViewDataManager.RefreshMenuItems;
    if Assigned(FAppModules.AccessControlManager()) then
      FAppModules.AccessControlManager.HideMenus;
    if Assigned(FAppModules.Changes()) then
      FAppModules.Changes.SetParameterChanges(FALSE);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSystemModelManager.DoOnHint(ASender: TObject);
const OPNAME = 'TSystemModelManager.DoOnHint';
begin
  try
    if Assigned(FAppModules.MainForm()) and Assigned(FAppModules.MainForm.PageControl.ActivePage) then
      TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).DoOnHint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.LanguageHasChanged: boolean;
const OPNAME = 'TSystemModelManager.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;;
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    if Result then
      Result := FAppModules.MainForm.MenuItemManager.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSystemModelManager.ProcessEvent(AEventType: integer; AData: TObject): boolean;
const OPNAME = 'TSystemModelManager.ProcessEvent';
begin
  Result := True;
  try
    case AEventType of
      CmeRefreshMenuItems        : RefreshMenuItems;
      CmeCopyToClipboard         : TAbstractTabSheet(FAppModules.MainForm.
                                     PageControl.ActivePage).DoCopyToClipboard;
      CmeExportToFile            : TAbstractTabSheet(FAppModules.MainForm.
                                     PageControl.ActivePage).DoExport;
      CmeDataViewJump            : FViewDataManager.DoJumpRequest(AData);
      CmeTabSheetCustomEvent     : TAbstractTabSheet(FAppModules.MainForm.
                                     PageControl.ActivePage).ProcessCustomEvent(AData);
      CmeCustomModelEvent        : ProcessCustomModelEvent(TModelMenuData(AData));
      CmeChangeParameter         : if Assigned(FAppModules.Changes()) then
                                     ProcessParameterChangeEvent;
      CmeMetaData                : if Assigned(FAppModules.MetaData()) then
                                     ProcessMetaDataEvent;
      CmeChangeListStationFilter : if Assigned(FAppModules.Changes()) then
                                      FAppModules.Changes.DoStationFilter(-1);

    else

      // If this point is reached the event was not handled.
      Result := False;
      raise Exception.CreateFmt('Unknown model manager event type [%d].', [integer(AEventType)]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemModelManager.OnTabChangeRequest(ASender: TObject; var AAllowChange: Boolean);
const OPNAME = 'TSystemModelManager.OnTabChangeRequest';
begin
  AAllowChange := False;
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    if Assigned(FAppModules.MainForm.PageControl.ActivePage) then
      AAllowChange := TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).CanTabChange;
    if AAllowChange then
    begin
      if(FAppModules.MainForm.MainForm.ActiveControl <> nil) then
      begin
        FAppModules.MainForm.MainForm.ActiveControl.Perform(CM_EXIT,0,0)
        //FAppModules.MainForm.MainForm.ActiveControl.Perform(WM_CHAR,VK_TAB,0)
        //PostMessage(LParentForm.Handle,WM_SYSKEYDOWN,VK_TAB,0);
        //PostMessage(LParentForm.Handle,WM_SYSKEYUP,VK_TAB,0);
      end;
      TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).SetMenuVisible(False);
      FAppModules.MainForm.ChildToolBar := nil;
      RefreshMenuItems;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSystemModelManager.OnTabHasChanged(ASender: TObject);
const OPNAME = 'TSystemModelManager.OnTabHasChanged';
begin
  try
    if not Assigned(FAppModules.MainForm()) then Exit;
    if Assigned(FAppModules.MainForm.PageControl.ActivePage) then
    begin
      TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).TabHasChanged;
      TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).SetMenuVisible(True);
      FAppModules.MainForm.ChildToolBar :=
        TCustomPanel(TAbstractTabSheet(FAppModules.MainForm.PageControl.ActivePage).ToolBar);
      RefreshMenuItems;
      FAppModules.MainForm.SaveCurrentTabIndex;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSystemModelManager.StudyHasChanged: boolean;
const OPNAME = 'TSystemModelManager.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    if Result and Assigned(FAppModules.MainForm()) then
       Result := FAppModules.MainForm.MenuItemManager.StudyHasChanged;
    if Result and Assigned(FAppModules.PrintManager()) then
       Result := FAppModules.PrintManager.StudyHasChanged;
    if Result and Assigned(FAppModules.AccessControlManager()) then
       Result := FAppModules.AccessControlManager.StudyHasChanged;
    if Result and Assigned(FAppModules.StudyDocumentManager()) then
       Result := FAppModules.StudyDocumentManager.StudyHasChanged;
    if Result and Assigned(FAppModules.Changes()) then
       Result := FAppModules.Changes.StudyHasChanged;
    if Result and Assigned(FViewDataManager) then
       if FLoadViewData then
         Result := FViewDataManager.StudyHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSystemModelManager.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if Result and Assigned(FAppModules.Changes()) then
      Result := FAppModules.Changes.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
    if Result and Assigned(FViewDataManager) then
      Result := FViewDataManager.StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.ProcessCustomModelEvent(AData: TModelMenuData): boolean;
const OPNAME = 'TSystemModelManager.ProcessCustomModelEvent';
begin
  Result := False;
  try
    raise Exception.CreateFmt('Unknown custom model event type [%d].', [integer(AData.Action)]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function  TSystemModelManager.ProcessParameterChangeEvent : boolean;
const OPNAME = 'TSystemModelManager.ProcessParameterChangeEvent';
begin
  Result := False;
  try
    raise Exception.CreateFmt('Parameter changes not allowed here!', []);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function  TSystemModelManager.ProcessMetaDataEvent : boolean;
const OPNAME = 'TSystemModelManager.ProcessMetaDataEvent';
begin
  Result := False;
  try
    raise Exception.CreateFmt('Meta Data not implemented here!', []);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.ModelDataLoaded: boolean;
const OPNAME = 'TSystemModelManager.ModelDataLoaded';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.EditFieldValue(AFieldName, AOldValue: string;
  AContextData: TStrings): boolean;
const OPNAME = 'TSystemModelManager.EditFieldValue';
begin
  inherited;
  Result := False;
  try
    EditFieldValue(AFieldName, AOldValue, '', AContextData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
function TSystemModelManager.EditFieldValue(AFieldName, AOldValue,
  ADefaultValue: string; AContextData: TStrings): boolean;
const OPNAME = 'TSystemModelManager.EditFieldValue';
var
  LFieldEditFunction : TFieldEditFunction;
  LFieldPropertyEditorForm : TFieldPropertyEditorForm;
  LNewValue : string;
  LFormModalResult : TModalResult;
begin
  Result := False;
  try
    LFieldEditFunction := GetFieldEditFunction(AFieldName);
    if Assigned(LFieldEditFunction) then
      LFieldEditFunction(AFieldName, ADefaultValue, AContextData)
    else
    begin
      LFieldPropertyEditorForm := TFieldPropertyEditorForm.CreateNew(nil);
      try
        LFieldPropertyEditorForm.SetFormCaption(Application.Title);
        LFieldPropertyEditorForm.EditBox.Text := ADefaultValue;
        LFieldPropertyEditorForm.EditedDataformat := edEditBox;
        LFieldPropertyEditorForm.ShowFormModal;
        LNewValue := LFieldPropertyEditorForm.EditBox.Text;
        LFormModalResult := LFieldPropertyEditorForm.FormModalResult;
      finally
        LFieldPropertyEditorForm.Free;
      end;
      if LFormModalResult <> mrCancel then
        if Assigned(FAppModules) and Assigned(FAppModules.Model()) then
          FAppModules.Model.UpdateFieldValue(AFieldName, LNewValue, AOldValue, AContextData);
      Result := true;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
}
function TSystemModelManager.DoesFieldHaveACustomEditor(
  AFieldName: String): Boolean;
const OPNAME = 'TSystemModelManager.DoesFieldHaveACustomEditor';
begin
  Result := false;
  try
    Result := Assigned(GetFieldEditFunction(AFieldName));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.GetFieldUpdateFunction(
  AFieldName: String): TFieldUpdateFunction;
const OPNAME = 'TSystemModelManager.GetFieldUpdateFunction';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := Low(FCustomFieldEditArray) to High(FCustomFieldEditArray) do
    begin
      if UpperCase(FCustomFieldEditArray[LIndex].FFieldName)= (UpperCase(AFieldName)) then
      begin
        if Assigned(FCustomFieldEditArray[LIndex].FUpdateFunction) then
        begin
          Result := FCustomFieldEditArray[LIndex].FUpdateFunction;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.GetFieldEditFunction(
  AFieldName: String): TFieldEditFunction;
const OPNAME = 'TSystemModelManager.GetFieldEditFunction';
var
  LIndex : integer;
begin
  Result := nil;
  try
    for LIndex := Low(FCustomFieldEditArray) to High(FCustomFieldEditArray) do
    begin
      if UpperCase(FCustomFieldEditArray[LIndex].FFieldName)= (UpperCase(AFieldName)) then
      begin
        if Assigned(FCustomFieldEditArray[LIndex].FEditFunction) then
        begin
          Result := FCustomFieldEditArray[LIndex].FEditFunction;
          Break;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSystemModelManager.DoModelDataHasChanged(AChangeLevel: TModelDataChangeLevel;
          AChangeType: TModelDataChangeType; AChangeAction: TModelDataChangeAction);
const OPNAME = 'TSystemModelManager.DoModelDataHasChanged';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.ValidateBusinessRule(ABusinessRule: integer;
  AObjectsArray: array of TObject; AErrorMessages: TStrings;AOptionalValues: string=''): boolean;
const OPNAME = 'TSystemModelManager.ValidateBusinessRule';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.DeleteModelDataViewer(AParent, AOwner: TObject): boolean;
const OPNAME = 'TSystemModelManager.DeleteModelDataViewer';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.ModelName: string;
const OPNAME = 'TSystemModelManager.ModelName';
begin
  Result := '';
  try
    Result := CSystem;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.PopulateDataList(AModelDataChangeType: TModelDataChangeType; AList: TList): boolean;
const OPNAME = 'TSystemModelManager.PopulateDataList';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{function TSystemModelManager.DoStationFilter(AChangeListID: integer): WordBool;
const OPNAME = 'TSystemModelManager.DoStationFilter';
begin
  Result := True;
  try //Do not insert code here
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TSystemModelManager.DeleteTabSheetTreeViewElement(AViewID,AParentIDsCommatext: string; AWeighting,
         AParentWeighting: integer): boolean;
const OPNAME = 'TSystemModelManager.DeleteTabSheetTreeViewElement';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSystemModelManager.CreateTabSheetTreeViewElement(AViewID,AParentIDsCommatext: string;
         AWeighting, AParentWeighting: integer; ACaption, ABitmapName, ADataType: string;
         ASelect: boolean; ATreeViewSheetName: TTreeViewSheetName): boolean;
const OPNAME = 'TSystemModelManager.CreateTabSheetTreeViewElement';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

