//
//
//  UNIT      : Contains TStudyAreaManager Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/02/06
//  COPYRIGHT : Copyright © 2002 DWAF
//
//

unit UStudyAreaManager;

interface

uses
  Classes,
  Vcl.ComCtrls,
  windows,
  Vcl.Forms,
  UStudyList,
  UOKCancelForm,
  UStudyArea,
  UStudyObjects,
  UAbstractObject,
  UStudyEditForm,
  UMenuItemManager,
  UDbTableDataManager,
  UStudyDatabaseAgent,
  UStudyAreaSelectionDialog,
  USystemModelLinkClasses,
  UUtilities,
  UAbstractModelObjects;

type
  TStudyAreaMenuItemManager = class(TMenuItemManager)
  public
    procedure AddMenuItems; override;
    procedure SetSelectStudyAreaEnabled(AEnabled: boolean);
  end;

  TStudyAreaManager = class(TAbstractStudyAreaManager)
  protected
    FStudyAreaMenuItemManager :TStudyAreaMenuItemManager;
    FStudyEditForm      : TfrmStudyEditForm;
    FStudyDatabaseAgent : TStudyDatabaseAgent;
    FStudyAreaSelectionDialog: TStudyAreaSelectionDialog;

    FStudyLabelCopy,
    FModelLabelCopy,
    FSubAreaLabelCopy,
    FScenarioLabelCopy : string;

    FStudyList: TStudyList;
    FTableDataManager:TDbTableDataManager;
    FExpandedNodeList : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure OnNewStudySelection(Sender: TObject);
    procedure OnEditStudySelection(Sender: TObject);
    procedure OnCopyStudySelection(Sender: TObject);
    procedure OnDeleteStudySelection(Sender: TObject);
    procedure OnStudySelectionAction(Sender: TObject);
    procedure OnExportStudyData(Sender: TObject);
    procedure OnImportStudyData(Sender: TObject);
    procedure OnBuildDatabase(Sender : TObject);
    procedure OnExportSystemData(Sender : TObject);
    procedure OnUnlockScenario(Sender : TObject);
    procedure OnCopyGISToClipboard(Sender : TObject);
    procedure OnExportGISToFile(Sender : TObject);
    //procedure OnGISPrint(Sender : TObject);
    procedure OnImportAllStudyData(Sender : TObject);
    procedure OnExportAllStudyData(Sender : TObject);
    //procedure OnLinkHydrologyStudies(Sender : TObject);
    //procedure OnUnLinkHydrologyStudies(Sender : TObject);

    procedure ReduceSecondAspectRatioToFirst(AX1, AY1 : integer; var AX2,AY2 : integer);
    procedure DoOnKeyDown ( Sender : TObject; var Key : word; Shift : TShiftState );
    procedure DoOnExpanded(Sender: TObject; Node: TTreeNode);
    procedure DoOnCollapsed(Sender: TObject; Node: TTreeNode);

    //procedure OnSubAreaPanelUpdateFromGIS(Sender: TObject);
    procedure OnSubAreaPanelUpdateDB(Sender: TObject);
    function ExecAddStudy(AStudyFields  :TStudyFields): boolean;
    function ExecUpdateStudy(AStudyFields  :TStudyFields): boolean;
    function ExecDeleteStudy(AStudyFields  :TStudyFields): boolean;
    function GetActionLevelFromNode(ANode: TTreeNode): TModelActionLevel;
    function PopulateStudyList(AStudyList: TStudyList; AAppModules: TAppModules): boolean;
    function RefreshStudySelectionDialog: boolean;
    procedure GetModelNames(AModelNames   : TStringList);
  public
    function BuildDatabase(AAppModules: TAppModules) : boolean;override;
    function ExportSystemData(AAppModules: TAppModules) : boolean;override;
    procedure SetSelectStudyAreaEnabled(AEnabled: boolean); override;
    function SelectStudy(AAppModules: TAppModules; var AStudyArea: TAbstractStudyArea): boolean; override;
    function AutoSelectStudy(AAppModules: TAppModules; var AStudyArea: TAbstractStudyArea): boolean; override;
    function SelectStudyDetails(AAppModules: TAppModules; var AStudyArea: TAbstractStudyArea; AStudyLabels: TStudyLabels): boolean; override;
    function ExportStudyData(AAppModules: TAppModules;ASelectionLevel: TModelActionLevel;
             AStudyArea:TAbstractStudyArea): boolean;override;
    function ImportStudyData(AAppModules : TAppModules): boolean; override;
    function ImportAllStudyData(AAppModules : TAppModules): boolean;
    function ExportAllStudyData(AAppModules : TAppModules): boolean;
    function CopyStudyData(AAppModules: TAppModules;ASelectionLevel: TModelActionLevel;
             AStudyArea:TAbstractStudyArea; AStudyFields: TStudyFields): boolean;override;
    function DeleteStudyData(AAppModules: TAppModules;ASelectionLevel: TModelActionLevel;
             AStudyArea:TAbstractStudyArea): boolean;override;
    procedure ExportChangeLists(AChangeListNumbersCommaText: string);override;
    procedure ImportChangeList;override;
  end;

implementation

uses
  SysUtils,
  System.UITypes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Clipbrd,
  Vcl.Printers,
  UConstants,
  UDataSetType,
  UAbstractComponent,
  UMainMenuEventType,
  UErrorHandlingOperations,
  Vcl.Dialogs;

const
  // File menu items.
  CSelectStudy         : array[0..1] of WideString = ('File','SelectStudyArea');
  //CLicenceModels         : array[0..1] of WideString = ('File','LicenceModels');
  CSelectStudyAreaSep  : array[0..1] of WideString = ('File','SelectStudyAreaSep');

{ TStudyAreaMenuItemManager }

procedure TStudyAreaMenuItemManager.AddMenuItems;
const OPNAME = 'TStudyAreaMenuItemManager.AddMenuItems';
begin
  inherited;
  try
    // File menu items.

    AddMenuItemEntry(CSelectStudy,        100, CmeSelectStudy);
    //AddMenuItemEntry(CLicenceModels,      101, CmeLicenceModels);
    AddMenuItemEntry(CSelectStudyAreaSep, 110, CmeSeparator);
    SetSelectStudyAreaEnabled(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaMenuItemManager.SetSelectStudyAreaEnabled(AEnabled: boolean);
const OPNAME = 'TStudyAreaMenuItemManager.SetSelectStudyAreaEnabled';
begin
  try
    if AEnabled then
    begin
      FAppModules.SetMenuItem(CSelectStudy, msEnable);
      //FToolBar.SetExportEnabled(AEnabled);
    end
    else
    begin
      FAppModules.SetMenuItem(CSelectStudy, msDisable, 'SelectStudyAreaDisabled');
      //FToolBar.SetExportEnabled(AEnabled);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.CreateMemberObjects;
const
  OPNAME = 'TStudyAreaManager.CreateMemberObjects';
begin
  inherited;
  try
    FStudyAreaMenuItemManager := TStudyAreaMenuItemManager.Create(FAppModules);
    FStudyEditForm  := nil;
    FStudyDatabaseAgent := TStudyDatabaseAgent.Create(FAppModules);
    FStudyAreaSelectionDialog := nil;
    FStudyList  := nil;
    FTableDataManager := TDbTableDataManager.Create(FAppModules);
    FExpandedNodeList := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.DestroyMemberObjects;
const
  OPNAME = 'TStudyAreaManager.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FStudyEditForm);
    FreeAndNil(FStudyDatabaseAgent);
    FreeAndNil(FStudyAreaSelectionDialog);
    FreeAndNil(FTableDataManager);
    FreeAndNil(FExpandedNodeList);
    FStudyList  := nil;
    FAppModules := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.SelectStudy(AAppModules: TAppModules; var AStudyArea: TAbstractStudyArea): boolean;
const
  OPNAME = 'TStudyAreaManager.SelectStudy';
var
  LStudyList : TStudyList;
  LMessage   : string;
  LMesgDlgResult : word;
  LStudyLabels : TStudyLabels;
  //LActionLevel: TModelActionLevel;
begin
  Result := False;
  try
    if Assigned(FStudyAreaSelectionDialog) then
       FreeAndNil(FStudyAreaSelectionDialog);

    AStudyArea := TStudyArea.Create(AAppModules);
    LStudyList := TStudyList.Create(AAppModules);
    try
      if not Assigned(FStudyAreaSelectionDialog) then
      begin
        FStudyAreaSelectionDialog := TStudyAreaSelectionDialog.Create(Application, AAppModules);
        //FStudyAreaSelectionDialog := TStudyAreaSelectionDialog.Create(nil, AAppModules);
        FStudyAreaSelectionDialog.ButtonPanel.NewButton.OnClick := OnNewStudySelection;
        FStudyAreaSelectionDialog.ButtonPanel.EditButton.OnClick := OnEditStudySelection;
        FStudyAreaSelectionDialog.ButtonPanel.DeleteButton.OnClick := OnDeleteStudySelection;
        FStudyAreaSelectionDialog.ButtonPanel.CopyButton.OnClick := OnCopyStudySelection;
        FStudyAreaSelectionDialog.StudyTreeView.OnKeyDown := DoOnKeyDown;
        FStudyAreaSelectionDialog.StudyTreeView.OnExpanded := DoOnExpanded;
        FStudyAreaSelectionDialog.StudyTreeView.OnCollapsed := DoOnCollapsed;

        //FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.BtnUpdateFromGIS.OnClick := OnSubAreaPanelUpdateFromGIS;
        FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.BtnUpdateDB.OnClick := OnSubAreaPanelUpdateDB;

        FStudyAreaSelectionDialog.mnuNewStudy.OnClick         := OnNewStudySelection;
        FStudyAreaSelectionDialog.mnuEditStudy.OnClick        := OnEditStudySelection;
        FStudyAreaSelectionDialog.mnuCopyStudy.OnClick        := OnCopyStudySelection;
        FStudyAreaSelectionDialog.mnuDeleteStudy.OnClick      := OnDeleteStudySelection;
        FStudyAreaSelectionDialog.mnuImportStudy.OnClick      := OnImportStudyData;
        FStudyAreaSelectionDialog.mnuExporStudy.OnClick       := OnExportStudyData;
        FStudyAreaSelectionDialog.mnuBuildDatabase.OnClick    := OnBuildDatabase;
        FStudyAreaSelectionDialog.mnuExportSystemData.OnClick := OnExportSystemData;
        FStudyAreaSelectionDialog.mnuUnlockScenario.OnClick   := OnUnlockScenario;
        FStudyAreaSelectionDialog.mnuCopyGISToClibboard.OnClick  := OnCopyGISToClipboard;
        FStudyAreaSelectionDialog.mnuExportGISToFile.OnClick  := OnExportGISToFile;
        //FStudyAreaSelectionDialog.mnuPrintGIS.OnClick         := OnGISPrint;
        FStudyAreaSelectionDialog.mnuImportAll.OnClick        := OnImportAllStudyData;
        FStudyAreaSelectionDialog.mnuExportAll.OnClick        := OnExportAllStudyData;
        //FStudyAreaSelectionDialog.mnuLinkHydrologyStudies.OnClick := OnLinkHydrologyStudies;
        //FStudyAreaSelectionDialog.mnuUnLinkHydrologyStudies.OnClick := OnUnLinkHydrologyStudies;
        FStudyAreaSelectionDialog.Initialise;
        FStudyAreaSelectionDialog.LanguageHasChanged;
      end;

      FStudyList  := LStudyList;
      if(FAppModules.StudyArea <> nil) then
        AStudyArea.AssignFrom(FAppModules.StudyArea);
      if PopulateStudyList(LStudyList, AAppModules) then
      begin
        FStudyAreaSelectionDialog.SetStudyList(LStudyList);
        FStudyAreaSelectionDialog.LanguageHasChanged;
        FStudyAreaSelectionDialog.PreSelectedNode :=
        FStudyAreaSelectionDialog.GetSelectedNode(AStudyArea.StudyAreaCode,AStudyArea.ModelSubCode,AStudyArea.SubAreaCode,AStudyArea.ScenarioCode);
        FStudyAreaSelectionDialog.SetExpandedNodes(FExpandedNodeList);

        while True do
        begin
          FStudyAreaSelectionDialog.ShowModal;
          FStudyAreaSelectionDialog.Hide;
          if (FStudyAreaSelectionDialog.ModalResult = mrOK) then
          begin
            if ((LStudyList.StudyArea.ModelCode = CYield) or (LStudyList.StudyArea.ModelCode = CPlanning)) and
                (LStudyList.StudyArea.ModelVersion <> '7')then
            begin
              LMessage :=  'The selected scenario version is no longer supported. Only version 7 is used.'+ #10#13+
                           'Please edit the scenario and update the version information.';
              WRMFMessageDialog(LMessage,mtConfirmation,[mbOK],[]);
              Continue;
            end
            else if LStudyList.StudyArea.DataImported then
            begin
              LMessage :=  'The selected scenario is imported and will be read only. '+ #10#13+
                           'You can make it editable by creating a copy of it. Continue ?';
              LMesgDlgResult := WRMFMessageDialog(LMessage,mtConfirmation,mbYesNoCancel,['Copy','OK']);
              if (LMesgDlgResult = mrCancel) then
                Continue;
              if (LMesgDlgResult = mrYes) then
              begin

                OnCopyStudySelection(nil);
                if (FStudyLabelCopy <> '') and (FModelLabelCopy <> '') and
                  (FSubAreaLabelCopy <> '') and (FScenarioLabelCopy <> '') then
                begin
                  LStudyLabels := TStudyLabels.Create;
                  try

                    LStudyLabels.StudyLabel    := FStudyLabelCopy;
                    LStudyLabels.ModelLabel    := FModelLabelCopy;
                    LStudyLabels.SubAreaLabel  := FSubAreaLabelCopy;
                    LStudyLabels.ScenarioLabel := FScenarioLabelCopy;
                    SelectStudyDetails(AAppModules, AStudyArea, LStudyLabels);
                  finally
                    LStudyLabels.Free;
                  end;
                end;

                Result := True;
                Break;

              end
              else
              begin
                AStudyArea.AssignFrom(LStudyList.StudyArea);
                Result := True;
                Break;
              end;
            end;
            AStudyArea.AssignFrom(LStudyList.StudyArea);
            Result := True;
            Break;
          end
          else Break;
        end;
      end;
    finally
      FreeAndNil(LStudyList);
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.DoOnExpanded(Sender: TObject;Node: TTreeNode);
const OPNAME = 'TStudyAreaManager.DoOnExpanded';
begin
  try
    case Node.Level of
    0 :
      begin
        if FExpandedNodeList.IndexOf(Node.Text) < 0 then
          FExpandedNodeList.Add(Node.Text);
      end;
    1 :
      begin
        if FExpandedNodeList.IndexOf(Node.Parent.Text+Node.Text+IntToStr(Node.Level)) < 0 then
          FExpandedNodeList.Add(Node.Parent.Text+Node.Text);
      end;
    2 :
      begin
        if FExpandedNodeList.IndexOf(Node.Parent.Parent.Text+Node.Parent.Text+Node.Text) < 0 then
          FExpandedNodeList.Add(Node.Parent.Parent.Text+Node.Parent.Text+Node.Text);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStudyAreaManager.DoOnCollapsed(Sender: TObject; Node: TTreeNode);
const OPNAME = 'TStudyAreaManager.DoOnCollapsed';
var
  LIndex : integer;
begin
  try
    case Node.Level of
    0 :
      begin
        LIndex := FExpandedNodeList.IndexOf(Node.Text);
        if (LIndex >= 0) then
          FExpandedNodeList.Delete(LIndex);
      end;
    1 :
      begin
        LIndex := FExpandedNodeList.IndexOf(Node.Parent.Text+Node.Text);
        if (LIndex >= 0) then
          FExpandedNodeList.Delete(LIndex);
      end;
    2 :
      begin
        LIndex := FExpandedNodeList.IndexOf(Node.Parent.Parent.Text+Node.Parent.Text+Node.Text);
        if (LIndex >= 0) then
          FExpandedNodeList.Delete(LIndex);
      end;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TStudyAreaManager.AutoSelectStudy(AAppModules: TAppModules; var AStudyArea: TAbstractStudyArea): boolean;
const
  OPNAME = 'TStudyAreaManager.AutoSelectStudy';
var
  LStudyLabels: TStudyLabels;
begin
  Result := False;
  try
    AStudyArea := TStudyArea.Create(AppModules);
    LStudyLabels := TStudyLabels.Create;
    try
      LStudyLabels.StudyLabel    := AAppModules.IniFile.ReadString('STUDY', 'Study',    '');
      LStudyLabels.ModelLabel    := AAppModules.IniFile.ReadString('STUDY', 'Model',    '');
      LStudyLabels.SubAreaLabel  := AAppModules.IniFile.ReadString('STUDY', 'SubArea',  '');
      LStudyLabels.ScenarioLabel := AAppModules.IniFile.ReadString('STUDY', 'Scenario', '');
      Result := SelectStudyDetails(AAppModules, AStudyArea, LStudyLabels);
    finally
      LStudyLabels.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.SelectStudyDetails(
  AAppModules: TAppModules; var AStudyArea: TAbstractStudyArea; AStudyLabels: TStudyLabels): boolean;
const
  OPNAME = 'TStudyAreaManager.SelectStudyDetails';
var
  LStudySelectionNodeData: TStudySelectionNodeData;
  LStudyList: TStudyList;
begin
  Result := False;
  try
    AStudyArea := TStudyArea.Create(AAppModules);
    LStudyList := TStudyList.Create(AAppModules);
    try
      if PopulateStudyList(LStudyList, AAppModules) then
      begin
        LStudySelectionNodeData :=
          LStudyList.FindFirstObjectByLabel(
            AStudyLabels.StudyLabel, AStudyLabels.ModelLabel, AStudyLabels.SubAreaLabel, AStudyLabels.ScenarioLabel);
        if Assigned(LStudySelectionNodeData) and (LStudySelectionNodeData.NodeLevel = nlScenario) then
        begin
          LStudyList.StudyArea.CopyFromScenarioDataObject(TScenarioDataObject(LStudySelectionNodeData));
          AStudyArea.AssignFrom(LStudyList.StudyArea);
          Result := True;
        end;
      end;
    finally
      LStudyList.Free;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.PopulateStudyList(AStudyList: TStudyList; AAppModules: TAppModules): boolean;
const
  OPNAME = 'TStudyAreaManager.PopulateStudyList';
var
  LCursor                     :TCursor;
  LAvailableStudiesDataSet  : TAbstractModelDataset;
  LStudyDocumentLinksDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
    LCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    AAppModules.Database.CreateDataset(integer(dtSelectStudyArea), LAvailableStudiesDataSet);
    try
      if Assigned(LAvailableStudiesDataSet) and
         Assigned(LAvailableStudiesDataSet.DataSet()) then
      begin
        AAppModules.Database.CreateDataset(integer(dtStudyDocumentsLinks), LStudyDocumentLinksDataSet);
        try
          if Assigned(LStudyDocumentLinksDataSet) and
             Assigned(LStudyDocumentLinksDataSet.DataSet()) then
          begin
            AStudyList.Populate(LAvailableStudiesDataSet, LStudyDocumentLinksDataSet);
            Result := True;
          end;
        finally
          Screen.Cursor := LCursor;
          LStudyDocumentLinksDataSet.Free;
        end;
      end;
    finally
      LAvailableStudiesDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnCopyStudySelection(Sender: TObject);
const
  OPNAME = 'TStudyAreaManager.OnCopyStudySelection';
var
  LStudyArea: TStudyArea;
  LScenarioDataObject: TScenarioDataObject;
  LNode: TTreeNode;
  LActionLevel: TModelActionLevel;
  LNodeLevel: TNodeLevel;
  LStudyFields: TStudyFields;
  LResult: boolean;
begin
  LResult := False;
  try
    if Assigned(FStudyAreaSelectionDialog) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView.Selected) then
    begin
      LNode := FStudyAreaSelectionDialog.StudyTreeView.Selected;
      LActionLevel := GetActionLevelFromNode(LNode);
      if(LActionLevel <> malNone) then
      begin
        LScenarioDataObject := FStudyAreaSelectionDialog.GetScenarioDataObjectFromNode(LNode);
        if Assigned(LScenarioDataObject) then
        begin
          LStudyArea := TStudyArea.Create(AppModules);
          LStudyFields := TStudyFields.Create;
          FStudyAreaSelectionDialog.IgnoreNodeChange := True;
          try
            LStudyArea.CopyFromScenarioDataObject(LScenarioDataObject);
            if FStudyAreaSelectionDialog.PopulateFieldsFromSelectionDialog(LNodeLevel,LStudyFields) then
            begin
              LResult := CopyStudyData(FAppModules, LActionLevel, LStudyArea, LStudyFields);
              FStudyLabelCopy := LStudyFields.FStudyLabel;
              FModelLabelCopy := LStudyArea.ModelLabel;
              FSubAreaLabelCopy := LStudyFields.FSubAreaLabel;
              FScenarioLabelCopy := LStudyFields.FScenarioLabel;
            end;
            if LResult then
            begin
              RefreshStudySelectionDialog;
              Sleep(500);
              FStudyAreaSelectionDialog.IgnoreNodeChange := False;
              FStudyAreaSelectionDialog.SelectedNode(LStudyFields.FStudyAreaName,LStudyFields.FModel,LStudyFields.FSubArea,LStudyFields.FScenario);
            end;
          finally
            FStudyAreaSelectionDialog.IgnoreNodeChange := False;
            LStudyArea.Free;
            LStudyFields.Free;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnDeleteStudySelection(Sender: TObject);
const
  OPNAME = 'TStudyAreaManager.OnDeleteStudySelection';
var
  LStudyArea          : TStudyArea;
  LScenarioDataObject : TScenarioDataObject;
  LNode               : TTreeNode;
  LActionLevel        : TModelActionLevel;
  LResult             : boolean;
begin
  LResult := False;
  try
    if Assigned(FStudyAreaSelectionDialog) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView.Selected) then
    begin
      LNode := FStudyAreaSelectionDialog.StudyTreeView.Selected;
      LActionLevel := GetActionLevelFromNode(LNode);
      if (LActionLevel <> malNone) then
      begin
        LScenarioDataObject := FStudyAreaSelectionDialog.GetScenarioDataObjectFromNode(LNode);
        if Assigned(LScenarioDataObject) then
        begin
          LStudyArea := TStudyArea.Create(AppModules);
          FStudyAreaSelectionDialog.IgnoreNodeChange := True;
          try
            LStudyArea.CopyFromScenarioDataObject(LScenarioDataObject);
            case LActionLevel of
              malStudy:
              begin
                LResult := DeleteStudyData(FAppModules, malStudy, LStudyArea);
              end;
              malModel:
              begin
                LResult := DeleteStudyData(FAppModules, malModel, LStudyArea);
              end;
              malSubArea:
              begin
                LResult := DeleteStudyData(FAppModules, malSubArea, LStudyArea);
              end;
              malScenarion:
              begin
                LResult := DeleteStudyData(FAppModules, malScenarion, LStudyArea);
              end;
            end;
            if LResult then
            begin
              RefreshStudySelectionDialog;
              Sleep(500);
              FStudyAreaSelectionDialog.IgnoreNodeChange := False;
              case LActionLevel of
                malSubArea   :
                  FStudyAreaSelectionDialog.SelectedNode(LStudyArea.StudyAreaCode,LStudyArea.ModelCode,'','');
                malScenarion :
                  FStudyAreaSelectionDialog.SelectedNode(LStudyArea.StudyAreaCode,LStudyArea.ModelCode,LStudyArea.SubAreaCode,'');
              end;
            end;
          finally
            FStudyAreaSelectionDialog.IgnoreNodeChange := False;
            LStudyArea.Free;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnEditStudySelection(Sender: TObject);
const
  OPNAME = 'TStudyAreaManager.OnEditStudySelection';
var
  LStudyFields  : TStudyFields;
  LNodeLevel    : TNodeLevel;
  LModelNames   : TStringList;
begin
  try
    if not Assigned(FStudyEditForm) then
    begin
      FStudyEditForm := TfrmStudyEditForm.Create(nil, FAppModules);
      FStudyEditForm.LanguageHasChanged;
      FStudyEditForm.btnSave.OnClick := OnStudySelectionAction;
    end;

    LStudyFields := TStudyFields.Create;
    try
      FStudyEditForm.StudyNameHasChanged := False;
      FStudyEditForm.SubAreHasChanged := False;
      FStudyEditForm.ScenarioHasChanged := False;
      FStudyEditForm.FormActionState := fasEdit;
      if FStudyAreaSelectionDialog.PopulateFieldsFromSelectionDialog(LNodeLevel,LStudyFields) then
      begin
        LModelNames := TStringList.Create;
        try
          GetModelNames(LModelNames);
          FStudyEditForm.cmbModel.Items.Assign(LModelNames);
        finally
          LModelNames.Free;
        end;

        FStudyEditForm.NodeLevel :=  LNodeLevel;
        FStudyEditForm.PopulateEditDialog(LStudyFields);
        FStudyEditForm.ShowModal;
        if(FStudyEditForm.ModalResult = mrOk) then
        begin
          FStudyAreaSelectionDialog.SelectedNode(LStudyFields.FStudyAreaName,LStudyFields.FModel,LStudyFields.FSubArea,LStudyFields.FScenario);
        end;
        FStudyEditForm.Hide;
      end;
    finally
      FreeAndNil(LStudyFields);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnNewStudySelection(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnNewStudySelection';
var
  LStudyFields  : TStudyFields;
  LNodeLevel    : TNodeLevel;
  LModelNames   : TStringList;
  LIndex        : integer;
  LCount        : integer;
begin
  try
    if not Assigned(FStudyEditForm) then
    begin
      FStudyEditForm := TfrmStudyEditForm.Create(nil, FAppModules);
      FStudyEditForm.LanguageHasChanged;
      FStudyEditForm.btnSave.OnClick := OnStudySelectionAction;
    end;

    LStudyFields := TStudyFields.Create;
    FStudyAreaSelectionDialog.IgnoreNodeChange := True;
    try
      FStudyEditForm.StudyNameHasChanged := False;
      FStudyEditForm.SubAreHasChanged := False;
      FStudyEditForm.ScenarioHasChanged := False;
      FStudyEditForm.FormActionState := fasAdd;
      if FStudyAreaSelectionDialog.PopulateFieldsFromSelectionDialog(LNodeLevel,LStudyFields) then
      begin
        LModelNames := TStringList.Create;
        try
          GetModelNames(LModelNames);
          FStudyEditForm.cmbModel.Items.Assign(LModelNames);

          if(LNodeLevel = nlModel) then
          begin
            FStudyAreaSelectionDialog.GetModelNamesForSelectedNode(LModelNames);
            for LCount := 0 to LModelNames.Count-1 do
            begin
              LIndex := FStudyEditForm.cmbModel.Items.IndexOf(LModelNames[LCount]);
              if(LIndex >= 0) then
                 FStudyEditForm.cmbModel.Items.Delete(LIndex);
            end;
          end;
        finally
          LModelNames.Free;
        end;

        FStudyEditForm.NodeLevel :=  LNodeLevel;
        FStudyEditForm.PopulateEditDialog(LStudyFields);
        FStudyEditForm.ShowModal;
        if(FStudyEditForm.ModalResult = mrOk) then
        begin
          FStudyEditForm.ValidateEditDialog(LStudyFields);
          FStudyAreaSelectionDialog.IgnoreNodeChange := False;
          FStudyAreaSelectionDialog.SelectedNode(LStudyFields.FStudyAreaName,LStudyFields.FModel,LStudyFields.FSubArea,LStudyFields.FScenario);
        end;
        FStudyEditForm.Hide;
      end;
    finally
      FStudyAreaSelectionDialog.IgnoreNodeChange := False;
      FreeAndNil(LStudyFields);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.GetModelNames(AModelNames: TStringList);
const OPNAME = 'TStudyAreaManager.GetModelNames';
var
  lUser         : TAbstractUser;
  lIndex        : integer;
  lUserModel    : string;
begin
  try
    AModelNames.Clear;
    lUser := FAppModules.User;
    if (Assigned(lUser)) then
    begin
      for lIndex := 0 to lUser.UserModelCount -1 do
      begin
        lUserModel := lUser.UserModelByIndex(lIndex);
        AModelNames.Add(lUserModel);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnStudySelectionAction(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnStudySelectionAction';
var
  LStudyFields  :TStudyFields;
  LResult: boolean;
begin
  try
    LResult := False;
    LStudyFields := TStudyFields.Create;
    try
      FStudyEditForm.ProgressBar1.Position := 0;
      if FStudyEditForm.ValidateEditDialog(LStudyFields) then
      begin
        case FStudyEditForm.FormActionState of
          fasEdit:
            begin
              LResult :=  ExecUpdateStudy(LStudyFields);
              if LResult then
                FStudyEditForm.ModalResult := 1;
            end;
          fasDelete:
            begin
              LResult := ExecDeleteStudy(LStudyFields);
              if LResult then
                FStudyEditForm.ModalResult := 1;
            end;
          fasAdd:
            begin
              LResult := ExecAddStudy(LStudyFields);
              if LResult then
                FStudyEditForm.ModalResult := 1;
            end;
        end;//Case
        if LResult then
          RefreshStudySelectionDialog;
       end;
    finally
      FreeAndNil(LStudyFields);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.ExecAddStudy(AStudyFields  :TStudyFields): boolean;
const OPNAME = 'TStudyAreaManager.ExecAddStudy';
var
  LEnabled : boolean;
begin
  Result := False;
  if not Assigned(AStudyFields) then
     raise Exception.Create('Study fields parameter is not yet assigned.');
  LEnabled := Assigned(FAppModules) and
             (FAppModules.User <> nil) and
             (FAppModules.User.UserRights in CUR_EditData);
  if not LEnabled then
    Exit;

  {if(AStudyFields.FModel = CHydrology) then
  begin
      if not Assigned(FTableDataManager) then
        FTableDataManager := TDbTableDataManager.Create(FAppModules);
    if not  FTableDataManager.CreateHydrologyNetwork(AStudyFields) then
      Exit;
  end;}

  try
    case FStudyEditForm.NodeLevel of
      nlStudy:
        begin
          FStudyEditForm.ShowProgres('',0,3);

          Result := FStudyDatabaseAgent.CreateStudy(AStudyFields.FModel,
                                                    AStudyFields.FStudyAreaName,
                                                    AStudyFields.FConsultant,
                                                    AStudyFields.FClient,
                                                    AStudyFields.FStudyLabel,
                                                    AStudyFields.FStudyAreaDescr,
                                                    AStudyFields.FStudyDate,
                                                    AStudyFields.FStudyNumber,
                                                    AStudyFields.FStudyShapeFileName);
          FStudyEditForm.ShowProgres('',1,3);
          if Result then
             Result := FStudyDatabaseAgent.CreateSubArea(AStudyFields.FModel,
                                                         AStudyFields.FStudyAreaName,
                                                         AStudyFields.FSubArea,
                                                         AStudyFields.FSubAreaLabel,
                                                         AStudyFields.FSubAreaDescr,
                                                         AStudyFields.FSubAreaShapeFileName );
          FStudyEditForm.ShowProgres('',2,3);
          if Result then
             Result := FStudyDatabaseAgent.CreateScenario(AStudyFields.FModel,
                                                         AStudyFields.FStudyAreaName,
                                                         AStudyFields.FSubArea,
                                                         AStudyFields.FScenario,
                                                         AStudyFields.FScenarioLabel,
                                                         AStudyFields.FScenarioDescr,
                                                         AStudyFields.FDataFilesPrefix,
                                                         AStudyFields.FDataFilesPath,
                                                         AStudyFields.FFilesLoaded,
                                                         AStudyFields.FEditable,
                                                         AStudyFields.FCalenderStartMonth,
                                                         AStudyFields.FVersion);
          FStudyEditForm.ShowProgres('',3,3);
        end;
      nlModel:
      begin
          FStudyEditForm.ShowProgres('',0,3);

          Result := FStudyDatabaseAgent.CreateStudy(AStudyFields.FModel,
                                                    AStudyFields.FStudyAreaName,
                                                    AStudyFields.FConsultant,
                                                    AStudyFields.FClient,
                                                    AStudyFields.FStudyLabel,
                                                    AStudyFields.FStudyAreaDescr,
                                                    AStudyFields.FStudyDate,
                                                    AStudyFields.FStudyNumber,
                                                    AStudyFields.FStudyShapeFileName);
          FStudyEditForm.ShowProgres('',1,3);
          if Result then
             Result := FStudyDatabaseAgent.CreateSubArea(AStudyFields.FModel,
                                                         AStudyFields.FStudyAreaName,
                                                         AStudyFields.FSubArea,
                                                         AStudyFields.FSubAreaLabel,
                                                         AStudyFields.FSubAreaDescr,
                                                         AStudyFields.FSubAreaShapeFileName );
          FStudyEditForm.ShowProgres('',2,3);
          if Result then
             Result := FStudyDatabaseAgent.CreateScenario(AStudyFields.FModel,
                                                         AStudyFields.FStudyAreaName,
                                                         AStudyFields.FSubArea,
                                                         AStudyFields.FScenario,
                                                         AStudyFields.FScenarioLabel,
                                                         AStudyFields.FScenarioDescr,
                                                         AStudyFields.FDataFilesPrefix,
                                                         AStudyFields.FDataFilesPath,
                                                         AStudyFields.FFilesLoaded,
                                                         AStudyFields.FEditable,
                                                         AStudyFields.FCalenderStartMonth,
                                                         AStudyFields.FVersion);
          FStudyEditForm.ShowProgres('',3,3);
      end;
      nlSubArea:
        begin
          FStudyEditForm.ShowProgres('',0,2);
          Result := FStudyDatabaseAgent.CreateSubArea(AStudyFields.FModel,
                                                     AStudyFields.FStudyAreaName,
                                                     AStudyFields.FSubArea,
                                                     AStudyFields.FSubAreaLabel,
                                                     AStudyFields.FSubAreaDescr,
                                                     AStudyFields.FSubAreaShapeFileName);
          FStudyEditForm.ShowProgres('',1,2);
          if Result then
             Result := FStudyDatabaseAgent.CreateScenario(AStudyFields.FModel,
                                                         AStudyFields.FStudyAreaName,
                                                         AStudyFields.FSubArea,
                                                         AStudyFields.FScenario,
                                                         AStudyFields.FScenarioLabel,
                                                         AStudyFields.FScenarioDescr,
                                                         AStudyFields.FDataFilesPrefix,
                                                         AStudyFields.FDataFilesPath,
                                                         AStudyFields.FFilesLoaded,
                                                         AStudyFields.FEditable,
                                                         AStudyFields.FCalenderStartMonth,
                                                         AStudyFields.FVersion);
          FStudyEditForm.ShowProgres('',2,2);
        end;
      nlScenario:
        begin
          FStudyEditForm.ShowProgres('',0,1);
          Result := FStudyDatabaseAgent.CreateScenario(AStudyFields.FModel,
                                                     AStudyFields.FStudyAreaName,
                                                     AStudyFields.FSubArea,
                                                     AStudyFields.FScenario,
                                                     AStudyFields.FScenarioLabel,
                                                     AStudyFields.FScenarioDescr,
                                                     AStudyFields.FDataFilesPrefix,
                                                     AStudyFields.FDataFilesPath,
                                                     AStudyFields.FFilesLoaded,
                                                     AStudyFields.FEditable,
                                                     AStudyFields.FCalenderStartMonth,
                                                     AStudyFields.FVersion);
          FStudyEditForm.ShowProgres('',1,1);
        end;
    end;

    if (AStudyFields.FModel = CRainfall) then
      FStudyDatabaseAgent.CheckAndCreateRainFallGaugesProject(AStudyFields.FModel,
                                                     AStudyFields.FStudyAreaName,
                                                     AStudyFields.FSubArea,
                                                     AStudyFields.FScenario,
                                                     AStudyFields.FScenarioLabel,
                                                     AStudyFields.FScenarioDescr,
                                                     AStudyFields.FDataFilesPrefix,
                                                     AStudyFields.FDataFilesPath,
                                                     AStudyFields.FFilesLoaded,
                                                     AStudyFields.FEditable,
                                                     AStudyFields.FCalenderStartMonth,
                                                     AStudyFields.FVersion);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.ExecDeleteStudy(AStudyFields  :TStudyFields): boolean;
const OPNAME = 'TStudyAreaManager.ExecDeleteStudy';
var
  LEnabled : boolean;
begin
  Result := False;
  try
    if not Assigned(AStudyFields) then
       raise Exception.Create('Study fields parameter is not yet assigned.');

    LEnabled := Assigned(FAppModules) and
               (FAppModules.User <> nil) and
               (FAppModules.User.UserRights in CUR_EditData);

  //LEnabled := LEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
  if not LEnabled then
    Exit;
    case FStudyEditForm.NodeLevel of
      nlStudy:
        begin
          Result := FStudyDatabaseAgent.DeleteStudy(FStudyEditForm.ShowProgres,
                                                    AStudyFields.FStudyAreaName);
        end;
      nlModel:
        begin
          Result := FStudyDatabaseAgent.DeleteModel(FStudyEditForm.ShowProgres,
                                                    AStudyFields.FModel,
                                                    AStudyFields.FStudyAreaName);
        end;
      nlSubArea:
        begin
          Result := FStudyDatabaseAgent.DeleteSubArea(FStudyEditForm.ShowProgres,
                                                      AStudyFields.FModel,
                                                      AStudyFields.FStudyAreaName,
                                                      AStudyFields.FSubArea);
        end;
      nlScenario:
        begin
          Result := FStudyDatabaseAgent.DeleteScenario(FStudyEditForm.ShowProgres,
                                                       AStudyFields.FModel,
                                                       AStudyFields.FStudyAreaName,
                                                       AStudyFields.FSubArea,
                                                       AStudyFields.FScenario);
        end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.ExecUpdateStudy(AStudyFields  :TStudyFields): boolean;
const OPNAME = 'TStudyAreaManager.ExecUpdateStudy';
var
  LEnabled : boolean;
begin
  Result := False;
  try
    if not Assigned(AStudyFields) then
       raise Exception.Create('Study fields parameter is not yet assigned.');
    LEnabled := Assigned(FAppModules) and
               (FAppModules.User <> nil) and
               (FAppModules.User.UserRights in CUR_EditData);
    //LEnabled := LEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    if not LEnabled then
      Exit;

    {if(AStudyFields.FModel = CHydrology) then
    begin
        if not Assigned(FTableDataManager) then
          FTableDataManager := TDbTableDataManager.Create(FAppModules);
      if not  FTableDataManager.UpdateHydrologyNetwork(AStudyFields) then
        Exit;
    end;}

    FStudyEditForm.ShowProgres('',0,1);
    case FStudyEditForm.NodeLevel of
      nlStudy:
        begin
          Result := FStudyDatabaseAgent.UpdateStudy(AStudyFields.FModel,
                                                    AStudyFields.FStudyAreaName,
                                                    AStudyFields.FConsultant,
                                                    AStudyFields.FClient,
                                                    AStudyFields.FStudyLabel,
                                                    AStudyFields.FStudyAreaDescr,
                                                    AStudyFields.FStudyDate,
                                                    AStudyFields.FStudyNumber,
                                                    AStudyFields.FStudyShapeFileName );
          FStudyEditForm.ShowProgres('',1,1);
        end;
      nlSubArea:
        begin
          Result := FStudyDatabaseAgent.UpdateSubArea(AStudyFields.FModel,
                                                     AStudyFields.FStudyAreaName,
                                                     AStudyFields.FSubArea,
                                                     AStudyFields.FSubAreaLabel,
                                                     AStudyFields.FSubAreaDescr,
                                                     AStudyFields.FSubAreaShapeFileName);
         FStudyEditForm.ShowProgres('',1,1);
        end;
      nlScenario:
        begin
          Result := FStudyDatabaseAgent.UpdateScenario(AStudyFields.FModel,
                                                     AStudyFields.FStudyAreaName,
                                                     AStudyFields.FSubArea,
                                                     AStudyFields.FScenario,
                                                     AStudyFields.FScenarioLabel,
                                                     AStudyFields.FScenarioDescr,
                                                     AStudyFields.FDataFilesPrefix,
                                                     AStudyFields.FDataFilesPath,
                                                     AStudyFields.FFilesLoaded,
                                                     AStudyFields.FEditable,
                                                     AStudyFields.FCalenderStartMonth,
                                                     AStudyFields.FVersion);
          FStudyEditForm.ShowProgres('',1,1);
        end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.RefreshStudySelectionDialog: boolean;
const
  OPNAME = 'TStudyAreaManager.RefreshStudySelectionDialog';
begin
  Result := False;
  try
    if PopulateStudyList(FStudyList, FAppModules) then
    begin
      FStudyAreaSelectionDialog.SetStudyList(FStudyList);
      FStudyAreaSelectionDialog.SetButtonStates;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.ExportStudyData(AAppModules: TAppModules;
  ASelectionLevel: TModelActionLevel;
  AStudyArea: TAbstractStudyArea): boolean;
const OPNAME = 'TStudyAreaManager.ExportStudyData';
begin
  Result := False;
  try
    if Assigned(AAppModules) and Assigned(AStudyArea) then
    begin
      Result := FTableDataManager.ExportStudyData(ASelectionLevel,AStudyArea);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.ImportStudyData (AAppModules : TAppModules): boolean;
const OPNAME = 'TStudyAreaManager.ImportStudyData';
var
  LEnabled : boolean;
begin
  Result := False;
  try
    if Assigned(AAppModules)then
    begin
      LEnabled := Assigned(FAppModules) and
                 (FAppModules.User <> nil) and
                 (FAppModules.User.UserRights in CUR_EditData);
      if not LEnabled then
        Exit;
      Result := FTableDataManager.ImportStudyData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.GetActionLevelFromNode(ANode: TTreeNode): TModelActionLevel;
const OPNAME = 'TStudyAreaManager.GetActionLevelFromNode';
begin
  Result := malNone;
  try
    case ANode.Level of
      0: Result := malStudy;
      1: Result := malModel;
      2: Result := malSubArea;
      3: Result := malScenarion;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.CopyStudyData(AAppModules: TAppModules;ASelectionLevel: TModelActionLevel;
             AStudyArea:TAbstractStudyArea; AStudyFields: TStudyFields): boolean;
const OPNAME = 'TStudyAreaManager.CopyStudyData';
var
  LEnabled : boolean;
begin
  Result := False;
  try
    if Assigned(FAppModules) and Assigned(AStudyArea) then
    begin
      LEnabled := Assigned(FAppModules) and
                 (FAppModules.User <> nil) and
                 (FAppModules.User.UserRights in CUR_EditData);
       //LEnabled := LEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
      if not LEnabled then
        Exit;
      Result := FTableDataManager.CopyStudyData(ASelectionLevel, AStudyArea, AStudyFields);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnExportStudyData(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnExportStudyData';
var
  LStudyArea: TStudyArea;
  LScenarioDataObject: TScenarioDataObject;
  LNode: TTreeNode;
  LActionLevel: TModelActionLevel;
begin
  try
    if Assigned(FStudyAreaSelectionDialog) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView.Selected) then
    begin
      LNode := FStudyAreaSelectionDialog.StudyTreeView.Selected;
      LActionLevel := GetActionLevelFromNode(LNode);
      if(LActionLevel <> malNone) then
      begin
        LScenarioDataObject := FStudyAreaSelectionDialog.GetScenarioDataObjectFromNode(LNode);
        if Assigned(LScenarioDataObject) then
        begin
          LStudyArea := TStudyArea.Create(AppModules);
          try
            LStudyArea.CopyFromScenarioDataObject(LScenarioDataObject);
            ExportStudyData(FAppModules,LActionLevel,LStudyArea);
          finally
            LStudyArea.Free;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnImportStudyData(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnImportStudyData';
begin
  try
    if (ImportStudyData(FAppModules)) then
      RefreshStudySelectionDialog;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnImportAllStudyData(Sender : TObject);
const OPNAME = 'TStudyAreaManager.OnImportAllStudyData';
begin
  try
    if (ImportAllStudyData(FAppModules)) then
      RefreshStudySelectionDialog;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnExportAllStudyData(Sender : TObject);
const OPNAME = 'TStudyAreaManager.OnExportAllStudyData';
begin
  try
    if (ExportAllStudyData(FAppModules)) then
      RefreshStudySelectionDialog;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyAreaManager.DeleteStudyData(AAppModules: TAppModules;
  ASelectionLevel: TModelActionLevel; AStudyArea: TAbstractStudyArea): boolean;
const OPNAME = 'TStudyAreaManager.DeleteStudyData';
var
  LEnabled : boolean;
begin
  Result := False;
  try
    if Assigned(AAppModules) and Assigned(AStudyArea) then
    begin
      LEnabled := Assigned(FAppModules) and
                 (FAppModules.User <> nil) and
                 (FAppModules.User.UserRights in CUR_EditData);
      if not LEnabled then
        Exit;
      Result := FTableDataManager.DeleteStudyData(ASelectionLevel,AStudyArea);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.SetSelectStudyAreaEnabled(AEnabled: boolean);
const OPNAME = 'TStudyAreaManager.SetSelectStudyAreaEnabled';
begin
  try
    FStudyAreaMenuItemManager.SetSelectStudyAreaEnabled(AEnabled);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnBuildDatabase ( Sender : TObject );
const OPNAME = 'TStudyAreaManager.OnBuildDatabase';
begin
  try
    BuildDatabase ( FAppModules );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStudyAreaManager.OnExportSystemData ( Sender : TObject );
const OPNAME = 'TStudyAreaManager.OnExportSystemData';
begin
  try
    ExportSystemData ( FAppModules );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStudyAreaManager.BuildDatabase(AAppModules: TAppModules) : boolean;
const OPNAME = 'TStudyAreaManager.BuildDatabase';
var
  LEnabled : boolean;
begin
  Result := False;
  try
    LEnabled := Assigned(AAppModules) and
                   (AAppModules.User <> nil) and
                   (AAppModules.User.UserRights = CUR_SysAdmin );
    if not LEnabled then
      Exit;
    Result := FTableDataManager.BuildDataBase;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStudyAreaManager.ExportSystemData(AAppModules: TAppModules) : boolean;
const OPNAME = 'TStudyAreaManager.ExportSystemData';
begin
  Result := False;
  try
    if Assigned(AAppModules)then
    begin
      Result := FTableDataManager.ExportSystemData;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStudyAreaManager.DoOnKeyDown(Sender : TObject; var Key : word; Shift: TShiftState);
const OPNAME = 'TStudyAreaManager.DoOnKeyDown';
begin
  try
    if (Key = VK_DELETE) and (FStudyAreaSelectionDialog <> nil) and
      (FStudyAreaSelectionDialog.ButtonPanel.DeleteButton.Enabled)  then
      OnDeleteStudySelection(nil);
    if (Key = VK_RETURN) and (FStudyAreaSelectionDialog <> nil) and
      (FStudyAreaSelectionDialog.ButtonPanel.OKButton.Enabled)  then
      FStudyAreaSelectionDialog.ButtonPanel.OKButton.OnClick(nil);
    if (Key = VK_ESCAPE) and (FStudyAreaSelectionDialog <> nil) and
      (FStudyAreaSelectionDialog.ButtonPanel.CancelButton.Enabled)  then
      FStudyAreaSelectionDialog.ButtonPanel.CancelButton.OnClick(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.OnUnlockScenario(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnUnlockScenario';
var
  LScenarioDataObject   : TScenarioDataObject;
  LModelCode,
  LStudyCode,
  LSubArea,
  LScenarioCode,
  LMsgStr : string;
begin
  try
    LScenarioDataObject         := TScenarioDataObject(FStudyAreaSelectionDialog.StudyTreeView.Selected.Data);

    LModelCode                  := LScenarioDataObject.SubArea.Model.Model;
    LStudyCode                  := LScenarioDataObject.SubArea.Model.Study.Study;
    LSubArea                    := LScenarioDataObject.SubArea.SubArea;
    LScenarioCode               := LScenarioDataObject.Scenario;

    if FAppModules.ScenarioLockManager.RequestScenarioUnlock(LModelCode,
                                                             LStudyCode,
                                                             LSubArea,
                                                             LScenarioCode) then
    begin
      FStudyAreaSelectionDialog.SetButtonStates;
      LMsgStr := 'Unlocked      ' + #13#10 +
                 'Model     : %s' + #13#10 +
                 'Study     : %s' + #13#10 +
                 'Sub Area : %s' + #13#10 +
                 'Scenario  : %s';
      LMsgStr  := Format(LMsgStr, [LModelCode, LStudyCode, LSubArea, LScenarioCode]);
      MessageDlg(LMsgStr, mtInformation, [mbOK], 0);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStudyAreaManager.OnCopyGISToClipboard(Sender : TObject);
const OPNAME = 'TStudyAreaManager.OnCopyGISToClipboard';
begin
  try
    //FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Output_CopyMap(1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStudyAreaManager.OnExportGISToFile(Sender : TObject);
const OPNAME = 'TStudyAreaManager.OnExportGISToFile';
var
  LDialog : TSaveDialog;
  LNode: TTreeNode;
  LActionLevel: TModelActionLevel;
  LScenarioDataObject: TScenarioDataObject;
  LPath   : string;
begin
  try
    LNode := FStudyAreaSelectionDialog.StudyTreeView.Selected;
    if(LNode <> nil) then
    begin
      LActionLevel := GetActionLevelFromNode(LNode);
      if(LActionLevel = malScenarion) then
      begin
        LScenarioDataObject := FStudyAreaSelectionDialog.GetScenarioDataObjectFromNode(LNode);
        if Assigned(LScenarioDataObject) then
        begin
          LPath := NetworkDiagramsPath +
                   ChopCharacters(LScenarioDataObject.SubArea.Model.Study.Study) + '\' +
                   ChopCharacters(LScenarioDataObject.SubArea.SubArea)   + '\' +
                   ChopCharacters(LScenarioDataObject.Scenario)  + '\';
          if not DirectoryExists(LPath) then
            ForceDirectories(LPath);

          LDialog := TSaveDialog.Create (nil);
          try
            LDialog.InitialDir  := LPath;
            LDialog.Options     := LDialog.Options + [ofNoChangeDir];
            LDialog.DefaultExt  := 'BMP';
            LDialog.Filter      := 'Bitmap files (*.bmp)|*.BMP';
            LDialog.FilterIndex := 1;
            if ( LDialog.Execute ) then
              //FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Output_ExportMap ( 1, LDialog.FileName, 1 );
          finally
            FreeAndNil ( LDialog );
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
(*
procedure TStudyAreaManager.OnGISPrint(Sender : TObject);
const OPNAME = 'TStudyAreaManager.OnGISPrint';
var
  LPrinterOrientation : TPrinterOrientation;
  LBitmap : TBitmap;
  LX1 : integer;
  LY1 : integer;
  LX2 : integer;
  LY2 : integer;
  lSpoolFile : string;
begin
  try
    lSpoolFile := GISCoversDirectory + 'SpoolFile.bmp';
    //FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Output_ExportMap(1, lSpoolFile, 1 );
    if ( Printer.Printers.Count > 0 ) then
    begin
      LBitmap := TBitmap.Create;
      try
        LBitmap.Width  := FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Width;
        LBitmap.Height := FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Height;
        LBitmap.LoadFromFile ( lSpoolFile );
        LPrinterOrientation := Printer.Orientation;
        try
          Printer.Orientation := poLandscape;
          LX1 := LBitmap.Width;
          LY1 := LBitMap.Height;
          LX2 := Printer.PageWidth;
          LY2 := Printer.PageHeight;
          ReduceSecondAspectRatioToFirst ( LX1,LY1,LX2,LY2 );
          if LX2 < Printer.PageWidth then
          begin
            LX1 := ((Printer.PageWidth - LX2) div 2);
            LY1 := 0;
            LX2 := LX2 + ((Printer.PageWidth - LX2) div 2 );
            LY2 := LY2;
          end;
          if LY2 < Printer.PageHeight then
          begin
            LX1 := 0;
            LY1:= ((Printer.PageHeight - LY2) div 2);
            LX2 := LX2;
            LY2 := LY2 + ((Printer.PageHeight - LY2) div 2);
          end;
          Printer.BeginDoc;
          Printer.Canvas.Stretchdraw(Rect(LX1, LY1 ,LX2 , LY2), LBitMap);
          Printer.EndDoc;
        finally
          Printer.Orientation := LPrinterOrientation;
        end;
      finally
        FreeAndNil(LBitmap);
        DeleteFile ( lSpoolFile );
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
 *)

procedure TStudyAreaManager.ReduceSecondAspectRatioToFirst(AX1, AY1 : integer; var AX2,AY2 : integer);
const OPNAME = 'TStudyAreaManager.ReduceSecondAspectRatioToFirst';
var
  LAspectRatio : Double;
begin
  // This function "reduces" the denominator or numerator of second ratio ( AX2/AY2 )to give the same
  // ratio as the first ( AX1/AY2 ).
  try
    if (AY1 <> 0) and (AY2 <> 0) then
    begin
      LAspectRatio := (AX1 / AY1);
      if LAspectRatio > (AX2 / AY2) then
      begin
        AY2 := Trunc(AX2 / LAspectRatio);
      end else begin
        AX2 := Trunc(AY2 * LAspectRatio);
      end;
    end else begin
      AX2 := 0;
      AY2 := 0;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TStudyAreaManager.ExportAllStudyData(AAppModules: TAppModules): boolean;
const OPNAME = 'TStudyAreaManager.ExportAllStudyData';
begin
  Result := False;
  try
    Result := FTableDataManager.ExportAllStudyData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStudyAreaManager.ImportAllStudyData(AAppModules: TAppModules): boolean;
const OPNAME = 'TStudyAreaManager.ImportAllStudyData';
begin
  Result := False;
  try
    Result := FTableDataManager.ImportAllStudyData;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStudyAreaManager.OnSubAreaPanelUpdateDB(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnSubAreaPanelUpdateDB';
var
  LCurrentNode : TTreeNode;
  LSubArea     : TSubAreaDataObject;
  LTopLeft     : double;
  LTopRight    : double;
  LBottomLeft  : double;
  LBottomRight : double;
  LVisioImageFile : string;
begin
  try
    LCurrentNode := FStudyAreaSelectionDialog.StudyTreeView.Selected;
    if(LCurrentNode <> nil) then
    begin
      if (LCurrentNode.Data = nil) then  Exit;
      if (TStudySelectionNodeData(LCurrentNode.Data).NodeLevel = nlSubArea) then
      begin
        LSubArea  :=  TSubAreaDataObject(LCurrentNode.Data);
        LTopLeft     := StrToFloat(Trim(FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.TopLeftCoordEdit.Text));
        LTopRight    := StrToFloat(Trim(FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.TopRightCoordEdit.Text));
        LBottomLeft  := StrToFloat(Trim(FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.BottomLeftCoordEdit.Text));
        LBottomRight := StrToFloat(Trim(FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.BottomRightCoordEdit.Text));

        if FStudyDatabaseAgent.UpdateSubAreaCoords(LSubArea.Model.Model,LSubArea.Model.Study.Study,LSubArea.SubArea,
                                                   LTopLeft,LTopRight,LBottomLeft,LBottomRight) then
        begin
          LSubArea.TopLeftCoord     := LTopLeft;
          LSubArea.TopRightCoord    := LTopRight;
          LSubArea.BottomLeftCoord  := LBottomLeft;
          LSubArea.BottomRightCoord := LBottomRight;
          LVisioImageFile := GetAppDataLocalDir+'\Network Diagrams\'+   //ExtractFilePath(ApplicationExeName) + 'Network Diagrams\'+
          ChopCharacters(FAppModules.StudyArea.StudyAreaCode)+'\'+ChopCharacters(FAppModules.StudyArea.SubAreaCode)
          +'\'+ChopCharacters(FAppModules.StudyArea.ScenarioCode);
          if ForceDirectories(LVisioImageFile) then
          begin

            LVisioImageFile := LVisioImageFile +'\'+FAppModules.StudyArea.ScenarioCode+'.glf';
           // FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Layers_SaveLayout(LVisioImageFile);
            UpdateGISShapeFilePath(LVisioImageFile);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
{
procedure TStudyAreaManager.OnSubAreaPanelUpdateFromGIS(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnSubAreaPanelUpdateFromGIS';
      GISWidth = 994;
      GISHeight = 663;
var
  LGISForm   : TOKCancelForm;
  LExTop,
  LExLeft,
  LExRight,
  LExBottom  : Double;
  LFileName : string;
  LOpenDialog : TOpenDialog;
  LScenarioData : TScenarioDataObject;
  LSubAreaData  : TSubAreaDataObject;
begin
  try
    if(FStudyAreaSelectionDialog.StudyTreeView.Selected <> nil) then
    begin
      LScenarioData := FStudyAreaSelectionDialog.GetScenarioDataObjectFromNode(FStudyAreaSelectionDialog.StudyTreeView.Selected);
      if(LScenarioData <> nil) then
      begin
        LSubAreaData := LScenarioData.SubArea;
        LGISForm := TOKCancelForm.CreateWithoutDFM(nil, FAppModules);
        LGISForm.Width := 1000;
        LGISForm.Height := 720;
        LGISForm.BorderIcons := [biSystemMenu];
        LGISForm.BorderStyle := bsToolWindow;
        LGISForm.Initialise;
        LGISForm.LanguageHasChanged;
        LExLeft := StrToFloat(FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.TopLeftCoordEdit.Text);
        LExTop := StrToFloat(FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.TopRightCoordEdit.Text);
        LExBottom := StrToFloat(FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.BottomRightCoordEdit.Text);
        LExRight := StrToFloat(FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.BottomLeftCoordEdit.Text);
        with FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer do
        begin
          if (MessageDlg(FAppModules.Language.GetString('Message.WRMFGLFFiles'),
              mtConfirmation,[mbYes, mbNo],0) = mrYes) then
          begin
            LOpenDialog := TOpenDialog.Create(nil);
            try
              LOpenDialog.DefaultExt  := 'GLF';
              LOpenDialog.Filter      := 'GLF Lists GLF files (*.glf)|*.GLF';
              LOpenDialog.FilterIndex := 1;
              if (LOpenDialog.Execute) then
                LFileName := ExtractFileName(LOpenDialog.FileName);
            finally
              LOpenDialog.Free;
            end;
            LFileName := GISCoversDirectory + LFileName;
            UpdateGISShapeFilePath(LFileName);
            ControlInterface.Layers_LoadLayout(LFileName,False);
            ControlInterface.Control_RefreshAll;
          end;
          Align := alNone;
          Parent := LGISForm;
          Left := 0;
          Top := 36;
          Width := GISWidth;
          Height := GISHeight;
          ControlInterface.Extent_Set(LExLeft, LExTop, LExRight, LExBottom);
          Application.ProcessMessages;
          Display_StatusBar  := True;
          Display_ToolBar    := True;
          Display_Legend     := True;
          Display_ScrollBars := False;
          Visible := True;
        end; // with
        if (LGISForm.ShowModal = mrOK) then
        begin
            FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.TopLeftCoordEdit.Text := FormatFloat('##.####', FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Extent_Left);
            FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.TopRightCoordEdit.Text := FormatFloat('##.####', FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Extent_Top);
            FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.BottomLeftCoordEdit.Text := FormatFloat('##.####', FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Extent_Right);
            FStudyAreaSelectionDialog.StudyDetailsPanel.SubAreaPanel.BottomRightCoordEdit.Text := FormatFloat('##.####', FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Extent_Bottom);
            LSubAreaData.TopLeftCoord     := FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Extent_Left;
            LSubAreaData.TopRightCoord    := FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Extent_Top;
            LSubAreaData.BottomLeftCoord  := FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Extent_Right;
            LSubAreaData.BottomRightCoord := FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer.Extent_Bottom;
        end;
        with FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel.GISViewer do
        begin
          Display_ToolBar := False;
          Display_Legend := False;
          Display_StatusBar := False;
          Parent := FStudyAreaSelectionDialog.GISStudyAreaSelectorPanel;
        end;
        LGISForm.Release;

      end;
    end;

  except on E: Exception do HandleError(E,OPNAME) end;
end;
 }
procedure TStudyAreaManager.ExportChangeLists(AChangeListNumbersCommaText: string);
const OPNAME = 'TStudyAreaManager.ExportChangeLists';
begin
  try
    FTableDataManager.ExportChangeLists(AChangeListNumbersCommaText);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyAreaManager.ImportChangeList;
const OPNAME = 'TStudyAreaManager.ImportChangeList';
begin
  try
    FTableDataManager.ImportChangeLists;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TStudyAreaManager.OnLinkHydrologyStudies(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnLinkHydrologyStudies';
var
  LEnabled : boolean;
  LStudyArea: TStudyArea;
  LScenarioDataObject: TScenarioDataObject;
  LNode: TTreeNode;
  LActionLevel: TModelActionLevel;
begin
  try
    LEnabled := Assigned(FAppModules) and
               (FAppModules.User <> nil) and
               (FAppModules.User.UserRights in CUR_EditData);
    if not LEnabled then
      Exit;

    if Assigned(FStudyAreaSelectionDialog) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView.Selected) then
    begin
      LNode := FStudyAreaSelectionDialog.StudyTreeView.Selected;
      LActionLevel := GetActionLevelFromNode(LNode);
      if(LActionLevel <> malNone) then
      begin
        LScenarioDataObject := FStudyAreaSelectionDialog.GetScenarioDataObjectFromNode(LNode);
        if Assigned(LScenarioDataObject) then
        begin
          LStudyArea := TStudyArea.Create(AppModules);
          try
            LStudyArea.CopyFromScenarioDataObject(LScenarioDataObject);
            if (NOT Assigned(FTableDataManager)) then
              FTableDataManager := TDbTableDataManager.Create(FAppModules);
             FTableDataManager.LinkHydrologyStudies(LActionLevel,LStudyArea);
             RefreshStudySelectionDialog;
             FStudyAreaSelectionDialog.SelectedNode(LStudyArea.StudyAreaCode,LStudyArea.ModelCode,LStudyArea.SubAreaCode,'');
          finally
            LStudyArea.Free;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

{procedure TStudyAreaManager.OnUnLinkHydrologyStudies(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnUnLinkHydrologyStudies';
var
  LEnabled : boolean;
  LStudyArea: TStudyArea;
  LScenarioDataObject: TScenarioDataObject;
  LNode: TTreeNode;
  LActionLevel: TModelActionLevel;
begin
  try
    LEnabled := Assigned(FAppModules) and
               (FAppModules.User <> nil) and
               (FAppModules.User.UserRights in CUR_EditData);
    if not LEnabled then
      Exit;

    if Assigned(FStudyAreaSelectionDialog) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView) and
       Assigned(FStudyAreaSelectionDialog.StudyTreeView.Selected) then
    begin
      LNode := FStudyAreaSelectionDialog.StudyTreeView.Selected;
      LActionLevel := GetActionLevelFromNode(LNode);
      if(LActionLevel <> malNone) then
      begin
        LScenarioDataObject := FStudyAreaSelectionDialog.GetScenarioDataObjectFromNode(LNode);
        if Assigned(LScenarioDataObject) then
        begin
          LStudyArea := TStudyArea.Create(AppModules);
          try
            LStudyArea.CopyFromScenarioDataObject(LScenarioDataObject);
            if (NOT Assigned(FTableDataManager)) then
              FTableDataManager := TDbTableDataManager.Create(FAppModules);
             FTableDataManager.UnLinkHydrologyStudies(LActionLevel,LStudyArea);
             RefreshStudySelectionDialog;
             FStudyAreaSelectionDialog.SelectedNode(LStudyArea.StudyAreaCode,LStudyArea.ModelCode,LStudyArea.SubAreaCode,'');
          finally
            LStudyArea.Free;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

end.



