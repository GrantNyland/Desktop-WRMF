unit UHydrologyEditDialog;

interface

uses Windows,
    SysUtils,
    Classes,
    Graphics,
    Forms,
    Controls,
    StdCtrls,
    Buttons,
    ExtCtrls,
    ComCtrls,
    ComObj,
    Dialogs,
    StrUtils,
    ImgList,
    {$WARN UNIT_PLATFORM OFF}
    FileCtrl,
    {$WARN UNIT_PLATFORM ON}
    ZipMstr,
    UConstants,
    UUtilities,
    UAbstractObject,
    UDataSetType,
    UStudyObjects,
    UStudyDatabaseAgent,
    UStudyArea,
    UStudyList,
    UDbTableDataManager,
    UErrorHandlingOperations,
    HydrologyCom_TLB,
    UHydroDBManager, System.ImageList;

type
  TfrmHydroEditDialog = class(TForm)
    Panel1: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    dlgWYRMFileSelector: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    ImportFileTreeView: TTreeView;
    Panel2: TPanel;
    btnImport: TButton;
    Panel3: TPanel;
    Label3: TLabel;
    Label1: TLabel;
    edtImportHydrologyPath: TEdit;
    btnHydrologyImportPath: TButton;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Label2: TLabel;
    edtExportFile: TEdit;
    Button2: TButton;
    Label4: TLabel;
    btnExport: TButton;
    btnDelete: TButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    CopyTreeView: TTreeView;
    Label6: TLabel;
    btnCopy: TButton;
    GroupBox2: TGroupBox;
    TreeView3: TTreeView;
    GroupBox3: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    edtNewNetCode: TEdit;
    edtNewVersion: TEdit;
    edtInputDir: TEdit;
    edtOutputDir: TEdit;
    chkNewDebugRequired: TCheckBox;
    edtNewDebugStart: TEdit;
    edtNewDebugEnd: TEdit;
    chkNewSumRequired: TCheckBox;
    edtNewSimStart: TEdit;
    edtNewSimEnd: TEdit;
    chkNewReadOnly: TCheckBox;
    btnCreate: TButton;
    rdoNetwork: TRadioButton;
    rdoNetworkAndStudy: TRadioButton;
    ImageList1: TImageList;
    GroupBox4: TGroupBox;
    NewTreeView: TTreeView;
    btnInputDir: TSpeedButton;
    btnOuputDir: TSpeedButton;
    NetworkListBox: TListBox;
    ExportFileDialog: TSaveDialog;
    edtNewNetworkCode: TEdit;
    Label18: TLabel;
    edtCopyNetworkCode: TEdit;
    Label19: TLabel;
    edtCopyScenarioLabel: TEdit;
    Label20: TLabel;
    Label21: TLabel;
    edtCopyVersion: TEdit;
    Label22: TLabel;
    edtCopyScenarioDescr: TEdit;
    TabSheet6: TTabSheet;
    InsertListBox: TListBox;
    Label23: TLabel;
    InsertTreeView: TTreeView;
    Label24: TLabel;
    btnInsert: TButton;
    rdoDeleteNetwork: TRadioButton;
    rdoDeleteStudy: TRadioButton;
    Panel4: TPanel;
    DeleteTreeView: TTreeView;
    rdoDeleteFromStudy: TRadioButton;
    rdoDeleteAll: TRadioButton;
    Label5: TLabel;
    Panel5: TPanel;
    Label25: TLabel;
    DeleteListBox: TListBox;
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetTreeViews(FTreeView : TTreeView);
    procedure ExpandTreeViewToAbsoluteIndex(FTreeView : TTreeView; FAbsoluteIndex : integer);
    procedure btnHydrologyImportPathClick(Sender: TObject);
    procedure ImportFileTreeViewClick(Sender: TObject);
    procedure ImportFileTreeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnImportClick(Sender: TObject);
    Procedure ToggleTreeViewCheckBoxes(Node             :TTreeNode;
                                       cUnChecked,
                                       cChecked         :integer);
    procedure FormCreate(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure rdoNetworkAndStudyClick(Sender: TObject);
    procedure rdoNetworkClick(Sender: TObject);
    procedure btnInputDirClick(Sender: TObject);
    procedure btnOuputDirClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ZipWholeFolder(AFileName : String; AFolder : String);
    procedure btnCopyClick(Sender: TObject);
    procedure OnCopyStudySelection(Sender: TObject);
    procedure SetStudyList(AStudyList: TStudyList; ATreeView : TTreeView);
    procedure CopyStudyFields(Old,New : TStudyFields);
    procedure GetNetworkCodes(ANetworkCodes : TStringList);
    procedure edtCopyNetworkCodeChange(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure AddNetworkToStudySubArea(ANetworkCode : String; ATreeView : TTreeView);
    procedure btnInsertClick(Sender: TObject);
    procedure TabSheet6Show(Sender: TObject);
    procedure DeleteScenarioByNode(ANode : TTreeNode);
    procedure rdoDeleteNetworkClick(Sender: TObject);
    procedure rdoDeleteStudyClick(Sender: TObject);
    procedure DeleteFromStudy;
    procedure DeleteFromNetwork;
    procedure btnDeleteClick(Sender: TObject);
    function DeleteFolder(APath : String) : integer;
    function PopulateFieldsFromSelectionDialog(var ANodeLevel: TNodeLevel; var AStudyFields: TStudyFields; ATreeView : TTreeView): boolean;
    function CopyStudyData(AAppModules: TAppModules;ASelectionLevel: TModelActionLevel;
             AStudyArea:TAbstractStudyArea; AStudyFields, ANewStudyFields: TStudyFields): boolean;
    function GetActionLevelFromNode(ANode: TTreeNode): TModelActionLevel;
    function GetScenarioDataObjectFromNode(ANode: TTreeNode): TScenarioDataObject;
    function PopulateStudyList(AStudyList: TStudyList; AAppModules: TAppModules): boolean;
    function RefreshStudySelectionDialog: boolean;
    function DeleteStudyData(AAppModules: TAppModules; ASelectionLevel: TModelActionLevel; AStudyArea: TAbstractStudyArea): boolean;
    function AddStudy(AStudyFields : TStudyFields; ANodeLevel : TNodeLevel) : boolean;
    procedure GetNetworkList;

  protected
    { Protected declarations }
   FTableDataManager: TDbTableDataManager;
   HStudyList : TStudyList;
   HTreeView : TTreeView;
   HNetWorkList : TStringList;
   FHydroDBManager : THydroDBManager;
   function AllNetworkCodes : WideString;
   function CreateNewNetwork (const ANetworkCode     : WideString;
                               AVersionNo             : Integer;
                               const AInputDir        : WideString;
                               const AOutputDir       : WideString;
                               const ADebugRequired   : WideString;
                               ADebugStartPeriod      : Integer;
                               ADebudEndPeriod        : Integer;
                               const ASummaryRequired : WideString;
                               ASimulationStartYear   : Integer;
                               ASimulationEndYear     : Integer;
                               AReadOnly              : Integer;
                               var ANetworkID         : Integer;
                               var AErrorMsg          : WideString): WordBool; safecall;

   function DeleteNetwork (const ANetworkCode : WideString;
                           var AErrorMsg      : WideString): WordBool; safecall;
   function CopyNetwork (const AOldNetworkCode : WideString;
                         const ANewNetworkCode : WideString;
                         var AErrorMsg         : WideString): WordBool; safecall;
   function ExportNetwork (const ANetworkCode : WideString;
                           const ADirectory   : WideString;
                           var AErrorMsg      : WideString): WordBool; safecall;
   function ImportNetwork (const ADirectory : WideString;
                           var ANetworkCode : WideString;
                           var AErrorMsg    : WideString): WordBool; safecall;




  public
    { Public declarations }

   //LModel : TAbstractModelManager;
   HAppModules : TAppModules;

  end;

var
  frmHydroEditDialog: TfrmHydroEditDialog;


Const
  cFlatUnCheck = 2;
  cFlatChecked = 1;

implementation

{$R *.dfm}

function TfrmHydroEditDialog.DeleteStudyData(AAppModules: TAppModules; ASelectionLevel: TModelActionLevel;
         AStudyArea: TAbstractStudyArea): boolean;
const OPNAME = 'TfrmHydroEditDialog.DeleteStudyData';
var
  LEnabled : boolean;
begin
  Result := False;
  try
    if Assigned(AAppModules) and Assigned(AStudyArea) then
    begin
      LEnabled := Assigned(AAppModules) and
                 (AAppModules.User <> nil) and
                 (AAppModules.User.UserRights in CUR_EditData);
      if not LEnabled then
        Exit;
      if not Assigned(FTableDataManager) then
         FTableDataManager := TDbTableDataManager.Create(AAppModules);
      Result := FTableDataManager.DeleteStudyData(ASelectionLevel,AStudyArea);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TfrmHydroEditDialog.SetStudyList(AStudyList: TStudyList; ATreeView : TTreeView);
const OPNAME = 'TfrmHydroEditDialog.SetStudyList';
var
  LStudyIndex,
  LModelIndex,
  LSubAreaIndex,
  LScenarioIndex: integer;
  LStudyNode,
  LModelNode,
  LSubAreaNode,
  LScenarioNode: TTreeNode;
begin
  try
    // Clear all the existing data.
    ATreeView.Items.Clear;
    HStudyList := AStudyList;

    // Add the tree nodes.
    for LStudyIndex := 0 to HStudyList.StudyCount - 1 do
    begin
      LStudyNode := ATreeView.Items.AddObject(nil,
        HStudyList[LStudyIndex].StudyLabel,
        HStudyList[LStudyIndex]);
      HStudyList[LStudyIndex].Node := LStudyNode;
      for LModelIndex := 0 to HStudyList[LStudyIndex].ModelCount - 1 do
      begin
        LModelNode := ATreeView.Items.AddChildObject(LStudyNode,
          HStudyList[LStudyIndex].Model[LModelIndex].ModelLabel,
          HStudyList[LStudyIndex].Model[LModelIndex]);
        HStudyList[LStudyIndex].Model[LModelIndex].Node := LModelNode;
        for LSubAreaIndex := 0 to HStudyList[LStudyIndex].Model[LModelIndex].SubAreaCount - 1 do
        begin
          LSubAreaNode := ATreeView.Items.AddChildObject(LModelNode,
            HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].SubAreaLabel,
            HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex]);
          HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].Node := LSubAreaNode;
          for LScenarioIndex := 0 to HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].ScenarioCount - 1 do
          begin
            LScenarioNode := ATreeView.Items.AddChildObject(LSubAreaNode,
              HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].Scenario[LScenarioIndex].ScenarioLabel,
              HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].Scenario[LScenarioIndex]);
            HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].Scenario[LScenarioIndex].Node := LScenarioNode;
          end;
        end;
      end;
    end;
    // Expand all the nodes.
    // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TfrmHydroEditDialog.RefreshStudySelectionDialog: boolean;
const OPNAME = 'TfrmHydroEditDialog.RefreshStudySelectionDialog';
var
  SelectedNode : Integer;
begin
  Result := False;
  try
    if not Assigned(HStudyList) then
      HStudyList := TStudyList.Create(HAppModules);
    if PopulateStudyList(HStudyList, HAppModules) then
    begin
      SelectedNode := CopyTreeView.Selected.AbsoluteIndex;
      SetStudyList(HStudyList,CopyTreeView);
      ExpandTreeViewToAbsoluteIndex(CopyTreeView,SelectedNode);
      SetStudyList(HStudyList,DeleteTreeView);
      ExpandTreeViewToAbsoluteIndex(DeleteTreeView,SelectedNode);
      SetStudyList(HStudyList,TreeView3);
      ExpandTreeViewToAbsoluteIndex(TreeView3,SelectedNode);
      SetStudyList(HStudyList,InsertTreeView);
      ExpandTreeViewToAbsoluteIndex(InsertTreeView,SelectedNode);
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.PopulateStudyList(AStudyList: TStudyList; AAppModules: TAppModules): boolean;
const OPNAME = 'TfrmHydroEditDialog.PopulateStudyList';
var
  LAvailableStudiesDataSet: TAbstractModelDataset;
  LStudyDocumentLinksDataSet: TAbstractModelDataset;
begin
  Result := False;
  try
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
          LStudyDocumentLinksDataSet.Free;
        end;
      end;
    finally
      LAvailableStudiesDataSet.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TfrmHydroEditDialog.GetScenarioDataObjectFromNode(ANode: TTreeNode): TScenarioDataObject;
const OPNAME = 'TfrmHydroEditDialog.GetScenarioDataObjectFromNode';
var LNode: TTreeNode;
begin
  Result := nil;
  try
    if Assigned(ANode) then
    begin
      LNode := ANode;
      case LNode.Level of
      0: begin
           if LNode.HasChildren then // Study level 0
           begin
             LNode := LNode.getFirstChild;
             if LNode.HasChildren then   //Model level 1
             begin
               LNode := LNode.getFirstChild;
               if LNode.HasChildren then   //Subarea level 2
               begin
                 LNode := LNode.getFirstChild; //Scenarion level 3
                 if Assigned(LNode.Data) then
                   Result := TScenarioDataObject(LNode.Data);
               end;
             end;
           end;
         end;
      1: begin
           if LNode.HasChildren then   //Model level 1
           begin
             LNode := LNode.getFirstChild;
             if LNode.HasChildren then   //Subarea level 2
             begin
               LNode := LNode.getFirstChild; //Scenarion level 3
               if Assigned(LNode.Data) then
                 Result := TScenarioDataObject(LNode.Data);
             end;
           end;
         end;
      2: begin
           if LNode.HasChildren then   //Subarea level 2
           begin
             LNode := LNode.getFirstChild; //Scenarion level 3
             if Assigned(LNode.Data) then
               Result := TScenarioDataObject(LNode.Data);
           end;
         end;
      3: begin
           if Assigned(LNode.Data) then  //Scenarion level 3
             Result := TScenarioDataObject(LNode.Data);
         end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.GetActionLevelFromNode(ANode: TTreeNode): TModelActionLevel;
const OPNAME = 'TfrmHydroEditDialog.GetActionLevelFromNode';
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

procedure TfrmHydroEditDialog.OnCopyStudySelection(Sender: TObject);
const OPNAME = 'TStudyAreaManager.OnCopyStudySelection';
var
  LStudyArea: TStudyArea;
  LScenarioDataObject: TScenarioDataObject;
  LNode: TTreeNode;
  LActionLevel: TModelActionLevel;
  LNodeLevel: TNodeLevel;
  LStudyFields, LNewStudyFields: TStudyFields;
  LResult: boolean;
  NewNetwork, OldNetwork, LErrMsg : WideString;

begin
  LResult := False;
  try
    if Assigned(CopyTreeView) and
       Assigned(CopyTreeView.Selected) then
    begin
      LNode := CopyTreeView.Selected;
      LActionLevel := GetActionLevelFromNode(LNode);
      if(LActionLevel <> malNone) then
      begin
        LScenarioDataObject := GetScenarioDataObjectFromNode(LNode);
        if Assigned(LScenarioDataObject) then
        begin
          LStudyArea := TStudyArea.Create(HAppModules);
          LStudyFields := TStudyFields.Create;
          LNewStudyFields := TStudyFields.Create;
          //FStudyAreaSelectionDialog.IgnoreNodeChange := True;
          try
            LStudyArea.CopyFromScenarioDataObject(LScenarioDataObject);
            if PopulateFieldsFromSelectionDialog(LNodeLevel,LStudyFields,CopyTreeView) then
            begin
              //LNewStudyFields := LStudyFields;
              CopyStudyFields(LStudyFields,LNewStudyFields);
              LNewStudyFields.FScenario := edtCopyNetworkCode.Text;
              //Showmessage(LStudyFields.FScenario);
              LNewStudyFields.FScenarioLabel := edtCopyScenarioLabel.Text;
              LNewStudyFields.FScenarioDescr := edtCopyScenarioDescr.Text;
              LNewStudyFields.FVersion := edtCopyVersion.Text;
              LResult := CopyStudyData(HAppModules, LActionLevel, LStudyArea, LStudyFields, LNewStudyFields);
              Application.ProcessMessages;
              if not LResult then exit;
              //GetHydrologyModel;
              OldNetwork := LStudyFields.FScenario;
              NewNetwork := LNewStudyFields.FScenario;
              if not CopyNetwork(OldNetwork,NewNetwork,LErrMsg) then
              begin
                MessageDlg('The WRMS Network copy operation failed'+#13+
                          LErrMsg,mtError,[mbOk],0);
              end;
            end;
            if LResult then
            begin
              edtCopyNetworkCode.Text := '';
              edtCopyScenarioLabel.Text := '';
              edtCopyVersion.Text := '';
              edtCopyScenarioDescr.Text := '';
              RefreshStudySelectionDialog;
              Sleep(500);
            end;
          finally
            LStudyArea.Free;
            LStudyFields.Free;
            LNewStudyFields.Free;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.CopyStudyData(AAppModules: TAppModules;ASelectionLevel: TModelActionLevel;
             AStudyArea:TAbstractStudyArea; AStudyFields, ANewStudyFields: TStudyFields): boolean;
const OPNAME = 'TfrmHydroEditDialog.CopyStudyData';
var
  LEnabled : boolean;
begin
  Result := False;
  try
    if Assigned(HAppModules) and Assigned(AStudyArea) then
    begin
      LEnabled := Assigned(AAppModules) and
                 (AAppModules.User <> nil) and
                 (AAppModules.User.UserRights in CUR_EditData);
       //LEnabled := LEnabled and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
      if not LEnabled then
        Exit;
      if not Assigned(FTableDataManager) then
         FTableDataManager := TDbTableDataManager.Create(AAppModules);
      result := FTableDataManager.CopyScenarioTableFromFieldList(AStudyFields,ANewStudyFields,malScenarion);

      //Result := FTableDataManager.CopyStudyData(ASelectionLevel, AStudyArea, AStudyFields);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TfrmHydroEditDialog.PopulateFieldsFromSelectionDialog(var ANodeLevel: TNodeLevel; var AStudyFields: TStudyFields; ATreeView : TTreeView): boolean;
const OPNAME = 'TStudyAreaSelectionDialog.PopulateFieldsFromSelectionDialog';
var
  LStudySelectionNodeData: TStudySelectionNodeData;
  LStudyDataObject: TStudyDataObject;
  LModelDataObject:  TModelDataObject;
  LSubAreaDataObject:  TSubAreaDataObject;
  LScenarioDataObject: TScenarioDataObject;
begin
  Result := False;
  try
    if not Assigned(AStudyFields) then
       raise Exception.Create('Study fields parameter is not yet assigned.');

    ANodeLevel := nlScenario;
    LStudyDataObject   := nil;
    LModelDataObject   := nil;
    LSubAreaDataObject := nil;
    LScenarioDataObject:= nil;

    AStudyFields.Reset;
    if Assigned(ATreeView.Selected) and
       Assigned(ATreeView.Selected.Data) then
    begin
      LStudySelectionNodeData := TStudySelectionNodeData(ATreeView.Selected.Data);
      ANodeLevel := LStudySelectionNodeData.NodeLevel;
      case ANodeLevel of
        nlStudy:
          begin
            LStudyDataObject    := TStudyDataObject(ATreeView.Selected.Data);
          end;
        nlModel:
          begin
            LStudyDataObject := TStudyDataObject(ATreeView.Selected.Parent.Data);
            LModelDataObject :=  TModelDataObject(ATreeView.Selected.Data);
          end;
        nlSubArea:
          begin
            LStudyDataObject := TStudyDataObject(ATreeView.Selected.Parent.Parent.Data);
            LModelDataObject :=  TModelDataObject(ATreeView.Selected.Parent.Data);
            LSubAreaDataObject :=  TSubAreaDataObject(ATreeView.Selected.Data);
          end;
        nlScenario:
          begin
            LStudyDataObject := TStudyDataObject(ATreeView.Selected.Parent.Parent.Parent.Data);
            LModelDataObject :=  TModelDataObject(ATreeView.Selected.Parent.Parent.Data);
            LSubAreaDataObject :=  TSubAreaDataObject(ATreeView.Selected.Parent.Data);
            LScenarioDataObject := TScenarioDataObject(ATreeView.Selected.Data);
          end;
      end;//case

      if Assigned(LStudyDataObject) then
      begin
        AStudyFields.FStudyAreaName  := LStudyDataObject.Study;
        AStudyFields.FStudyDate      := LStudyDataObject.StudyDate;
        AStudyFields.FConsultant     := LStudyDataObject.Consultant;
        AStudyFields.FClient         := LStudyDataObject.Client;
        //Not in object fields.
        AStudyFields.FStudyNumber    := '1';
        AStudyFields.FStudyLabel     := LStudyDataObject.StudyLabel;
        AStudyFields.FStudyAreaDescr := LStudyDataObject.StudyDescr;
        AStudyFields.FStudyShapeFileName := LStudyDataObject.StudyShapeFileName;
      end;

      if Assigned(LModelDataObject) then
      begin
        AStudyFields.FModel := LModelDataObject.Model;
        AStudyFields.FSubModel := LModelDataObject.SubModel;
      end;

      if Assigned(LSubAreaDataObject) then
      begin
        AStudyFields.FSubArea             := LSubAreaDataObject.SubArea;
        AStudyFields.FSubAreaLabel        := LSubAreaDataObject.SubAreaLabel;
        AStudyFields.FSubAreaDescr        := LSubAreaDataObject.SubAreaDescr;
        AStudyFields.FSubAreaShapeFileName:= LSubAreaDataObject.SubAreaShapeFileName;
        AStudyFields.FTopLeftCoord        := LSubAreaDataObject.TopLeftCoord;
        AStudyFields.FTopRightCoord       := LSubAreaDataObject.TopRightCoord;
        AStudyFields.FBottomLeftCoord     := LSubAreaDataObject.BottomLeftCoord;
        AStudyFields.FBottomRightCoord    := LSubAreaDataObject.BottomRightCoord;
      end;

      if Assigned(LScenarioDataObject) then
      begin
        AStudyFields.FScenario        := LScenarioDataObject.Scenario;
        AStudyFields.FScenarioLabel   := LScenarioDataObject.ScenarioLabel;
        AStudyFields.FScenarioDescr   := LScenarioDataObject.ScenarioDescr;
        AStudyFields.FDataFilesPrefix := LScenarioDataObject.DataFilesPrefix;
        AStudyFields.FDataFilesPath   := LScenarioDataObject.DataFilesPath;
        AStudyFields.FFilesLoaded     := LScenarioDataObject.FilesLoaded;
        AStudyFields.FEditable        := not LScenarioDataObject.DataImported;
        AStudyFields.FCalenderStartMonth     := LScenarioDataObject.CalenderStartMonth;
        AStudyFields.FVersion                := LScenarioDataObject.Version;
      end;
    end
    else
    ANodeLevel := nlStudy;

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;



procedure TfrmHydroEditDialog.CancelBtnClick(Sender: TObject);
const OPNAME = 'TfrmImportHydroDBDialog.CancelBtnClick';
begin
  try
    self.Close;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.OKBtnClick(Sender: TObject);
const OPNAME = 'TfrmImportHydroDBDialog.OKBtnClick';
begin
  try
    ModalResult := mrOK;
    CancelBtn.Click;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.FormClose(Sender: TObject; var Action: TCloseAction);
Const OPNAME = 'TfrmHydroEditDialog.FormClose';
begin
  try
    if Assigned(HStudyList) then FreeAndNil(HStudyList);
    if Assigned(HTreeView) then FreeAndNil(HTreeView);
    if Assigned(HNetworkList) then FreeAndNil(HNetworkList);
    if Assigned(FTableDataManager) then FreeAndNil(FTableDataManager);
    if Assigned(FHydroDBManager) then FreeAndNil(FHydroDBManager);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.btnHydrologyImportPathClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.btnHydrologyImportPathClick';
var
  ZipMaster : TZipMaster;
  i : integer;
  LList : TList;
  LTreeNode : TTreeNode;
  LHelpString : String;
begin
  try
    dlgWYRMFileSelector.Filter := 'Zip Files|*.zip|XML Files|*.xml';
    dlgWYRMFileSelector.Execute;
    dlgWYRMFileSelector.Filter := '';
    if dlgWYRMFileSelector.FileName <> '' then
       edtImportHydrologyPath.Text := dlgWYRMFileSelector.FileName;
    LHelpString := ExtractFileName(edtImportHydrologyPath.Text);
    edtNewNetworkCode.Text := LeftStr(LHelpString,Length(LHelpString)-4);

    if ExtractFileExt(edtImportHydrologyPath.Text) = '.zip' then
    begin
     ZipMaster := TZipmaster.Create(nil);
     ZipMaster.ZipFilename := edtImportHydrologyPath.Text;
     ZipMaster.List;
     LList := ZipMaster.ZipContents;
     ImportFileTreeView.items.Clear;
     LTreeNode := ImportFileTreeView.Items.Add(ImportFileTreeView.TopItem,ExtractFileName(ZipMaster.ZipFilename));
     LTreeNode.StateIndex := cFlatChecked;
     ImportFileTreeView.TopItem := ImportFileTreeView.Items.GetFirstNode;
     //ImportFileTreeView.TopItem.StateIndex := cFlatChecked;

     for i := 0 to LList.Count -1 do
     begin
       if Uppercase(ExtractFileExt(PZipDirEntry(LList[i]).FileName)) = '.XML' then
         begin
           LTreeNode := ImportFileTreeView.Items.AddChild(ImportFileTreeView.TopItem,PZipDirEntry(LList[i]).FileName);
           LTreeNode.StateIndex := cFlatChecked;
         end;
     end;
     ZipMaster.Free;
     ImportFileTreeView.FullExpand;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.ImportFileTreeViewClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.ImportFileTreeViewClick';
var
  P:TPoint;
  i : integer;
begin
  try
    GetCursorPos(P);
    P := ImportFileTreeView.ScreenToClient(P);

    {
    htAbove	Above the client area.
    htBelow	Below the client area.
    htNowhere	Inside the control, but not on an item.
    htOnItem	On an item, its text, or its bitmap.
    htOnButton	On a button.
    htOnIcon	On an icon.
    htOnIndent	On the indented area of an item.
    htOnLabel	On a label.
    htOnRight	On the right side of an item.
    htOnStateIcon	On a state icon or bitmap associated with an item.
    htToLeft	To the left of the client area.
    htToRight	To the right of the client area.
    }
    {  temp := ImportFileTreeView.GetHitTestInfoAt(P.X,P.Y);
      if htOnStateIcon  in temp then Showmessage('htOnStateIcon');
      if htAbove in temp then showmessage('htAbove');
      if htBelow in temp then showmessage('htBelow');
      if htNowhere in temp then showmessage('htNowhere');
      if htOnItem in temp then showmessage('htOnItem');
      if htOnButton in temp then showmessage('htOnButton');
      if htOnIcon in temp then showmessage('htOnIcon');
      if htonIndent in temp then showmessage('htOnIndent');
      if htOnLabel in temp then showmessage('htOnLabel');
      if htOnRight in temp then showmessage('htOnRight');
      if htToLeft in temp then showmessage('htToLeft');
      if htToRight in temp then showmessage('htToRight');
    }

    if (htOnStateIcon in ImportFileTreeView.GetHitTestInfoAt(P.X,P.Y)) then
      ToggleTreeViewCheckBoxes(ImportFileTreeView.Selected, cFlatUnCheck,  cFlatChecked);

    if ImportFileTreeView.Selected.HasChildren then
    begin
      for i:= 0 to ImportFileTreeView.Selected.Count -1 do
      begin
        ImportFileTreeView.Selected.Item[i].StateIndex := ImportFileTreeView.Selected.StateIndex;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TfrmHydroEditDialog.ImportFileTreeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
const OPNAME = 'TfrmHydroEditDialog.ImportFileTreeViewKeyDown';
begin
  try
    if (Key = VK_SPACE) and  Assigned(ImportFileTreeView.Selected) then
      ToggleTreeViewCheckBoxes(ImportFileTreeView.Selected,cFlatUnCheck,cFlatChecked);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.btnImportClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.btnImportClick';

var
  i              : Integer;
  ZipMaster      : TZipMaster;
  FilesToUnZip   : TStringList;
  LNode          : TTreeNode;
  LTempDirectory : String;
  LNetworkCode   : WideString;
  LErrMsg        : WideString;
  LExistingNetworkCodes : TStringList;
begin
  try
    if Trim(edtImportHydrologyPath.Text) = '' then exit;
    if Trim(edtNewNetworkCode.Text) = '' then exit;

    LExistingNetworkCodes := TStringList.Create;
    try
      LExistingNetworkCodes.Delimiter := ',';
      LExistingNetworkCodes.DelimitedText := AllNetworkCodes;
      for i:= 0 to LExistingNetworkCodes.Count -1 do
      begin
        if Uppercase(Trim(edtNewNetworkCode.Text)) = UpperCase(LExistingNetworkCodes.Strings[i]) then
        begin
          MessageDlg('The network code allready exists.'+#13+
                     'Please try a different name...',mtError,[mbOk],0);
          FreeAndNil(LExistingNetworkCodes);
          Exit;
        end;
      end;

      LNetworkCode := Trim(edtNewNetworkCode.Text);
    finally
      FreeAndNil(LExistingNetworkCodes);
    end;

    if ImportFileTreeView.Items.Count < 1 then exit;
    LTempDirectory := ExtractFilePath(edtImportHydrologyPath.Text) +'Temp\';

    LNode := ImportFileTreeView.Items.GetFirstNode;

    FilesToUnzip := TStringList.Create;
    try
      for i:=0 to LNode.Count -1 do
      begin
        if LNode.Item[i].StateIndex = cFlatChecked then
        begin
          FilesToUnzip.Add(LNode.Item[i].Text);
        end;
      end;

      if FilesToUnZip.Count > 0 then
      begin
        ZipMaster := TZipMaster.Create(nil);
        try
          if FileExists(edtImportHydrologyPath.Text) then
          begin
            ZipMaster.ZipFilename := edtImportHydrologyPath.Text;
            ZipMaster.FSpecArgs.Assign(FilesToUnZip);
            if DirectoryExists(LTempDirectory) then
              DeleteFolder(LTempDirectory);

            ForceDirectories(LTempDirectory);
            ChDir(LTempDirectory);
            ZipMaster.ExtrBaseDir := LTempDirectory;

            ZipMaster.ExtrOptions := [ExtrOverWrite];
            ZipMaster.Extract;

            LTempDirectory := LeftStr(LTempDirectory,Length(LTempDirectory)-1);
            if not ImportNetwork(LTempDirectory,LNetworkCode,LErrMsg) then
              MessageDlg('There was an error during import.' + #13 + LErrMsg,mtError,[mbOk],0);
            LTempDirectory := LTempDirectory + '\';
            ChDir(ExtractFilePath(edtImportHydrologyPath.Text));
          end;
        finally
          FreeAndNil(ZipMaster);
        end;
      end;
    finally
      FilesToUnzip.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.DeleteFolder(APath: String): integer;
const OPNAME = 'TfrmHydroEditDialog.DeleteFolder';
var
  SearchRec:TSearchRec;
  FolderToDelete : String;
begin
// Result = 0 {Everything worked correctly}
// Result = 1 {Directory doesn't exist}
// Result = 2 {Directory is not empty}
Result := -1;
  try
    if not DirectoryExists(Apath) then
    begin
      Result := 1;
      exit;
    end;

    FolderToDelete := APath + '*.*';
    if (FindFirst(FolderToDelete, faAnyfile-faDirectory, SearchRec) = 0) then
    begin
      repeat
        DeleteFile(APath+SearchRec.Name);
      until (FindNext(SearchRec) <> 0);
      FindClose(SearchRec);
      RmDir(APath);
      if IOResult <> 0 then
      begin
        Result := 0;
      end
      else
      begin
        Result := 2
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.ToggleTreeViewCheckBoxes(Node: TTreeNode; cUnChecked, cChecked: integer);
const OPNAME = 'TfrmHydroEditDialog.ToggleTreeViewCheckBoxes';
begin
  try
    if Assigned(Node) then
    begin
      if Node.StateIndex = cUnChecked then
        Node.StateIndex := cChecked
      else if Node.StateIndex = cChecked then
        Node.StateIndex := cUnChecked;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.FormCreate(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.FormCreate';
begin
  try
    ImportFileTreeView.StateImages := ImageList1;
    HTreeView := TTreeView.Create(nil);
    //ICreatedModel := false;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.RadioButton4Click(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.RadioButton4Click';
begin
  try
    GroupBox3.Visible := RadioButton4.Checked;
    GroupBox2.Visible := not RadioButton4.Checked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.RadioButton3Click(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.RadioButton3Click';
begin
  try
    groupbox2.Visible := radiobutton3.Checked;
    Groupbox3.Visible := not radiobutton3.Checked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.SetTreeViews(FTreeView: TTreeView);
const OPNAME = 'TfrmHydroEditDialog.SetTreeViews';
begin
  try
    DeleteTreeView.Items := FTreeView.Items;
    ExpandTreeViewToAbsoluteIndex(DeleteTreeView,FTreeView.Selected.AbsoluteIndex);

    CopyTreeView.Items := FTreeView.Items;
    ExpandTreeViewToAbsoluteIndex(CopyTreeView,FTreeView.Selected.AbsoluteIndex);

    TreeView3.Items := FTreeView.Items;
    ExpandTreeViewToAbsoluteIndex(TreeView3,FTreeView.Selected.AbsoluteIndex);

    InsertTreeView.Items := FTreeView.Items;
    ExpandTreeViewToAbsoluteIndex(InsertTreeView,FTreeView.Selected.AbsoluteIndex);

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.ExpandTreeViewToAbsoluteIndex(FTreeView: TTreeView; FAbsoluteIndex : Integer);
const  OPNAME = 'TfrmHydroEditDialog.ExpandTreeViewToAbsoluteIndex';

var
  i,
  j,
  k,
  l : integer;
begin
  try
    for i := 0 to FTreeView.Items.Count -1 do
    begin
      if FTreeView.Items[i].AbsoluteIndex = FAbsoluteIndex then
      begin
       FTreeView.Items[i].Selected := true;
       FTreeView.Items[i].MakeVisible;
       break;
      end;

      for j := 0 to FTreeView.Items[i].Count -1 do
      begin
        if FTreeView.Items[i].Item[j].AbsoluteIndex = FAbsoluteIndex then
        Begin
           FTreeView.Items[i].Item[j].Selected := true;
           FTreeView.Items[i].Item[j].MakeVisible;
           break
        end;

        for k := 0 to FTreeView.Items[i].Item[j].Count -1 do
        begin
          if FTreeView.Items[i].Item[j].Item[k].AbsoluteIndex = FAbsoluteIndex then
          begin
            FTreeView.Items[i].Item[j].Item[k].Selected := true;
            FTreeView.Items[i].Item[j].Item[k].MakeVisible;
            break;
          end;

          for l := 0 to FTreeView.Items[i].Item[j].Item[k].Count -1 do
          begin
            if FTreeView.Items[i].Item[j].Item[k].Item[l].AbsoluteIndex = FAbsoluteIndex then
            begin
              FTreeView.Items[i].Item[j].Item[k].Item[l].Selected := true;
              FTreeView.Items[i].Item[j].Item[k].Item[l].MakeVisible;
              break;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.rdoNetworkAndStudyClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.rdoNetworkAndStudyClick';
var
  LTreeView : TTreeview;
begin
  try
    LTreeView := DeleteTreeView;
    GroupBox4.Enabled := True;
    NewTreeView.Items := LTreeView.Items;
    ExpandTreeViewToAbsoluteIndex(NewTreeView,LTreeView.Selected.AbsoluteIndex);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.rdoNetworkClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.rdoNetworkClick';
begin
  try
    GroupBox4.Enabled := false;
    NewTreeView.Items.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.btnInputDirClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.btnInputDirClick';
      SELDIRHELP = 1000;
var
  LDir : String;
begin
  try
    LDir := ExtractFilePath(Application.ExeName);
    if SelectDirectory(LDir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP) then
      edtInputDir.Text := LDir;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.btnOuputDirClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.btnOuputDirClick';
      SELDIRHELP = 1000;
var
  LDir : String;
begin
  try
    LDir := ExtractFilePath(Application.ExeName);
    if SelectDirectory(LDir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP) then
        edtOutputDir.Text := LDir;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.btnExportClick(Sender: TObject);
Const OPNAME = 'TfrmHydroEditDialog.btnExportClick';
var
  i : integer;
  LNetWorkCode : WideString;
  LDirectory : WideString;
  LTempDirectory : WideString;
  LErrMsg : WideString;
  PathExist : boolean;
begin
  try
    LDirectory := ExtractFilePath(edtExportFile.Text);
    LTempDirectory := LDirectory + 'temp\';
    if FileExists(edtExportFile.Text) then
    begin
      if MessageDlg('This will overwrite the existing ['+edtExportFile.Text+']',mtConfirmation,[mbYes,mbNo],0) = mrNo then
        exit;
    end;

    if DirectoryExists(ExtractFilePath(edtExportFile.Text)) then
    begin
      if not DirectoryExists(LTempDirectory) then
      begin
        MkDir(LTempDirectory);
        if IOResult <> 0 then
        Begin
          ShowMessage('Cannot create temporary directory.' + #13 + 'Export aborted..');
          Exit;
        end;
      end;
    end;

    PathExist := DirectoryExists(LTempDirectory);
    if (edtExportFile.Text <> '') and PathExist then
    begin
      for i := 0 to NetworkListBox.Items.Count -1 do
      begin
        if NetworkListBox.Selected[i] then
        begin
          LNetWorkCode := NetworkListBox.Items.Strings[i];
          if not ExportNetwork(LNetWorkCode,LTempDirectory,LErrMsg) then
          ShowMessage(LErrMsg);
        end;
      end;
    end;
    
    ZipWholeFolder(edtExportFile.Text, LTempDirectory);
    DeleteFolder(LTempDirectory);
    if FileExists(edtExportFile.Text) then
    begin
      MessageDlg('The system has created your export file..',mtInformation,[mbOK],0);
    end
    else
    begin
      MessageDlg('The system was not able to create your export file..'+#13+
      'Please check that there is data to export and that you have [write] access to your file',mtError,[mbOk],0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.Button2Click(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.Button2Click';
begin
  try
    if ExportFileDialog.Execute then
      edtExportFile.Text := ExportFileDialog.FileName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.ZipWholeFolder(AFileName : String; AFolder : String);
const OPNAME = 'TfrmHydroEditDialog.ZipWholeFolder';
var
  SearchRec:TSearchRec;
  ZipMaster : TZipMaster;
  PathToFolder : String;
  FolderToZip : String;
  FilesToZip : TStringList;
begin
  try
    ZipMaster := TZipMaster.Create(nil);
    FilesToZip := TStringList.Create;
    ZipMaster.ZipFilename := AFileName;
    PathToFolder := AFolder;
    FolderToZip := PathToFolder + '*.*';

    if (FindFirst(FolderToZip, faAnyfile-faDirectory, SearchRec) = 0) then
     begin
       repeat
         FilesToZip.Add(PathToFolder+SearchRec.Name);
       until (FindNext(SearchRec) <> 0);
         FindClose(SearchRec);
     end;
    ZipMaster.FSpecArgs := FilesToZip;
    ZipMaster.AddCompLevel := 9;
    ZipMaster.Add;

    ZipMaster.Free;
    FilesToZip.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.btnCopyClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.btnCopyClick';
var LStudyFields  : TStudyFields;
    LNodeLevel : TNodeLevel;
begin
  try
    LStudyFields := TStudyFields.Create;
    PopulateFieldsFromSelectionDialog(LNodeLevel,LStudyFields,CopyTreeView);

    if (LStudyFields.FModel <> CHydrology) or (LStudyFields.FScenario = '') then
     Begin
       ShowMessage('You must select a Hydrology Network..');
       FreeAndNil(LStudyFields);
       exit;
     end;
    OnCopyStudySelection(self);
    //RefreshStudySelectionDialog;


    FreeAndNil(LStudyFields);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.CopyStudyFields(Old, New: TStudyFields);
const OPNAME = 'TfrmHydroEditDialog.CopyStudyFields';
begin
  try
    New.FModel := Old.FModel;
    New.FSubModel := Old.FSubModel;
    New.FStudyAreaName := Old.FStudyAreaName;
    New.FSubArea := Old.FSubArea;
    New.FScenario := Old.FScenario;
    New.FStudyDate := Old.FStudyDate;
    New.FConsultant := Old.FConsultant;
    New.FClient := Old.FClient;
    New.FStudyNumber := Old.FStudyNumber;
    New.FStudyLabel := Old.FStudyLabel;
    New.FStudyAreaDescr := Old.FStudyAreaDescr;
    New.FStudyShapeFileName := Old.FStudyShapeFileName;
    New.FSubAreaLabel := Old.FSubAreaLabel;
    New.FSubAreaDescr := Old.FSubAreaDescr;
    New.FSubAreaShapeFileName := Old.FSubAreaShapeFileName;
    New.FTopLeftCoord := Old.FTopLeftCoord;
    New.FTopRightCoord := Old.FTopRightCoord;
    New.FBottomLeftCoord := Old.FBottomLeftCoord;
    New.FBottomRightCoord := Old.FBottomRightCoord;
    New.FScenarioLabel := Old.FScenarioLabel;
    New.FScenarioDescr := Old.FScenarioLabel;
    New.FDataFilesPrefix := Old.FDataFilesPrefix;
    New.FDataFilesPath := Old.FDataFilesPath;
    New.FFilesLoaded := Old.FFilesLoaded;
    New.FCalenderStartMonth := Old.FCalenderStartMonth;
    New.FEditable := Old.FEditable;
    New.FVersion := Old.FVersion;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.GetNetworkCodes(ANetworkCodes : TStringList);
const OPNAME = 'TfrmHydroEditDialog.GetNetworkCodes';
//var LNetworkList : TStrings;
begin
  try
  //  LNetworkList := TStrings.Create;
  //  if not Assigned(HNetWorkList) then HNetworkList := TStringList.Create;
  //  LNetworkList.Delimiter := ',';
  //  LNetworkList.DelimitedText := AllNetworkCodes;
  //  ANetworkCodes.Clear;
  //  ANetworkCodes.AddStrings(LNetworkList);
  //  FreeAndNil(LNetworkList);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.edtCopyNetworkCodeChange(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.edtCopyNetworkCodeChange';
  var i : integer;
      LEnabled : Boolean;
begin
  try
  LEnabled := false;
  GetNetworkList;
  //if not Assigned(HNetWorkList) then
  //  begin
      //HNetworkList := TStringList.Create;
  //  end;
  for i:=0 to HNetWorkList.Count -1 do
    begin
      if (UpperCase(HNetWorkList.Strings[i]) = UpperCase(edtCopyNetworkCode.Text))
        then
          begin
            LEnabled := false;
            break;
          end
        else
          begin
            LEnabled := true;

          end;

    end;

    //LEnabled := (LEnabled and (Trim(edtCopyScenarioLabel.Text) <> '') and ((Trim(edtCopyVersion.Text) <> '')) and ((Trim(edtCopyVersion.Text) <> '')) );
    if (Trim(edtCopyScenarioLabel.Text) <> '') then LEnabled := LEnabled and true
                                               else LEnabled := LEnabled and false;
    if (Trim(edtCopyScenarioDescr.Text) <> '') then LEnabled := LEnabled and true
                                               else LEnabled := LEnabled and false;
    if (Trim(edtCopyVersion.Text) <> '') then LEnabled := LEnabled and true
                                         else LEnabled := LEnabled and false;
    btnCopy.Enabled := LEnabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.TabSheet2Show(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.TabSheet2Show';
begin
  try
   NetworkListBox.Items.Delimiter := ',';
   NetworkListBox.Items.DelimitedText := AllNetworkCodes;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.btnCreateClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.RdoOnlyNeworkClick';
var
  NewNetworkCode,
  NewInputDir,
  NewOutputDir,
  NewDebugRequired,
  NewSumRequired,
  LErrMsg : WideString;

  NewVersion,
  NewDebugStart,
  NewDebugEnd,
  NewSimStart,
  NewSimEnd,
  NewReadOnly,
  RetNetworkID,
  SelectedNode : Integer;
begin
  try
    NewNetworkCode := edtNewNetCode.Text;
    NewInputDir := edtInputDir.Text;
    NewOutputDir := edtOutputDir.Text;
    if chkNewDebugRequired.Checked then NewDebugRequired := '1'
                                   else NewDebugRequired := '0';
    if chkNewSumRequired.Checked then NewSumRequired := '1'
                                   else NewSumRequired := '0';
    Try
      NewVersion := StrToInt(Trim(edtNewVersion.Text));
    Except on EConvertError do NewVersion := -1; end;
    Try
    NewDebugStart := StrToInt(Trim(edtNewDebugStart.Text));
    Except on EConvertError do NewDebugStart := -1; end;
    try
    NewDebugEnd := StrToInt(Trim(edtNewDebugEnd.Text));
    Except on EConvertError do NewDebugEnd := -1; end;
    try
    NewSimStart := StrToInt(Trim(edtNewSimStart.Text));
    Except on EConvertError do NewSimStart := -1; end;
    try
    NewSimEnd := StrToInt(Trim(edtNewSimEnd.Text));
    Except on EConvertError do NewSimEnd := -1; end;

    if chkNewReadOnly.Checked then NewReadOnly := 1
                              else NewReadOnly := 0;
    LErrMsg := '';
    if NewSimEnd = -1 then LErrMsg := 'Simulation End Period';
    if NewSimStart = -1 then LErrMsg := 'Simulation Start Period';
    if NewDebugEnd = -1 then LErrMsg := 'Debug Start Period';
    if NewDebugStart = -1 then LErrMsg := 'Debug Start Period';
    if NewVersion = -1 then LErrMsg := 'Version Number';
    if Trim(NewOutputDir) = '' then LErrMsg := 'Output Directory';
    if Trim(NewInputDir) = '' then LErrMsg := 'Input Directory';
    if Trim(NewNetworkCode) = '' then LErrMsg := 'Network Code';

    if LErrMsg <> '' then
      begin
        MessageDlg('Some fields were not entered correctly.'+#13+#13+'['+LErrMsg+']',mtError,[mbOk],0);
        exit;
      end;
    LErrMsg := '';
    //GetHydrologyModel;
    if not CreateNewNetwork(NewNetworkCode,NewVersion,NewInputDir,
           NewOutputDir,NewDebugRequired,NewDebugStart,NewDebugEnd,NewSumRequired,
           NewSimStart,NewSimEnd,NewReadOnly,RetNetworkID,LErrMsg) then
    begin
       Messagedlg('An error occured trying to create the new network'+#13+#13+LErrMsg,mtError,[mbOk],0);
    end
    else
    begin
      if (rdoNetworkAndStudy.Checked) and (LErrMsg = '') then
      begin
        AddNetworkToStudySubArea(NewNetworkCode,NewTreeView);
      end;

      if not Assigned(HStudyList) then HStudyList := TStudyList.Create(HAppModules);
      if PopulateStudyList(HStudyList, HAppModules) then
      begin
        SelectedNode := NewTreeView.Selected.AbsoluteIndex;
        SetStudyList(HStudyList,NewTreeView);
        ExpandTreeViewToAbsoluteIndex(NewTreeView,SelectedNode);
      end;

      MessageDlg('The Network ['+NewNetworkCode+'] has been created.',mtInformation,[mbOk],0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.AddStudy(AStudyFields: TStudyFields; ANodeLevel : TNodeLevel) : boolean;
const OPNAME = 'TfrmHydroEditDialog.AddStudy';
var
  LEnabled : boolean;
  LStudyDatabaseAgent : TStudyDatabaseAgent;
begin
  Result := False;
  try
    if not Assigned(AStudyFields) then
       raise Exception.Create('Study fields parameter is not yet assigned.');
    LEnabled := Assigned(HAppModules) and
               (HAppModules.User <> nil) and
               (HAppModules.User.UserRights in CUR_EditData);
    if not LEnabled then
      Exit;

    LStudyDatabaseAgent := TStudyDatabaseAgent.Create(HAppModules);
    try
      case ANodeLevel of
        nlStudy:
          begin
            Result := LStudyDatabaseAgent.CreateStudy(AStudyFields.FModel,
                                                      AStudyFields.FStudyAreaName,
                                                      AStudyFields.FConsultant,
                                                      AStudyFields.FClient,
                                                      AStudyFields.FStudyLabel,
                                                      AStudyFields.FStudyAreaDescr,
                                                      AStudyFields.FStudyDate,
                                                      AStudyFields.FStudyNumber,
                                                      AStudyFields.FStudyShapeFileName);
            if Result then
               Result := LStudyDatabaseAgent.CreateSubArea(AStudyFields.FModel,
                                                           AStudyFields.FStudyAreaName,
                                                           AStudyFields.FSubArea,
                                                           AStudyFields.FSubAreaLabel,
                                                           AStudyFields.FSubAreaDescr,
                                                           AStudyFields.FSubAreaShapeFileName );
            if Result then
               Result := LStudyDatabaseAgent.CreateScenario(AStudyFields.FModel,
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
          end;
        nlModel:
        begin

            Result := LStudyDatabaseAgent.CreateStudy(AStudyFields.FModel,
                                                      AStudyFields.FStudyAreaName,
                                                      AStudyFields.FConsultant,
                                                      AStudyFields.FClient,
                                                      AStudyFields.FStudyLabel,
                                                      AStudyFields.FStudyAreaDescr,
                                                      AStudyFields.FStudyDate,
                                                      AStudyFields.FStudyNumber,
                                                      AStudyFields.FStudyShapeFileName);
            if Result then
               Result := LStudyDatabaseAgent.CreateSubArea(AStudyFields.FModel,
                                                           AStudyFields.FStudyAreaName,
                                                           AStudyFields.FSubArea,
                                                           AStudyFields.FSubAreaLabel,
                                                           AStudyFields.FSubAreaDescr,
                                                           AStudyFields.FSubAreaShapeFileName );
            if Result then
               Result := LStudyDatabaseAgent.CreateScenario(AStudyFields.FModel,
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
        end;
        nlSubArea:
          begin
            Result := LStudyDatabaseAgent.CreateSubArea(AStudyFields.FModel,
                                                       AStudyFields.FStudyAreaName,
                                                       AStudyFields.FSubArea,
                                                       AStudyFields.FSubAreaLabel,
                                                       AStudyFields.FSubAreaDescr,
                                                       AStudyFields.FSubAreaShapeFileName);
            if Result then
               Result := LStudyDatabaseAgent.CreateScenario(AStudyFields.FModel,
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
          end;
        nlScenario:
          begin
            Result := LStudyDatabaseAgent.CreateScenario(AStudyFields.FModel,
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
          end;
      end;
    finally
      FreeAndNil(LStudyDatabaseAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.AddNetworkToStudySubArea(ANetworkCode: String; ATreeView: TTreeView);
const OPNAME = 'TfrmHydroEditDialog.AddNetworkToStudySubArea';
var
  LStudyFields   : TStudyFields;
  LNodeLevel     : TNodeLevel;
  SelectedNode   : Integer;
  LStudyIndex,
  LModelIndex,
  LSubAreaIndex,
  LScenarioIndex : integer;
begin
  try
    //malSubArea
    LStudyFields := TStudyFields.Create;
    try
      PopulateFieldsFromSelectionDialog(LNodeLevel,LStudyFields,InsertTreeView);

      if (LStudyFields.FModel <> CHydrology) or ((LStudyFields.FSubArea = '') and (LStudyFields.FModel = CHydrology)) then
      Begin
        ShowMessage('You must select a Hydrology Network Sub Area..');
        FreeAndNil(LStudyFields);
        exit;
      end;


      //LStudyFields.FModel,
      //LStudyFields.FStudyAreaName,
      //LStudyFields.FSubArea,
      LStudyFields.FScenario := ANetworkCode;
      LStudyFields.FScenarioLabel := ANetworkCode;
      LStudyFields.FScenarioDescr := ANetworkCode;
      //LStudyFields.FDataFilesPrefix,
      //LStudyFields.FDataFilesPath,
      LStudyFields.FFilesLoaded := False;
      LStudyFields.FEditable := true;
      LStudyFields.FCalenderStartMonth := 10;
      LStudyFields.FVersion := '1.0';
      {
      Showmessage(LStudyFields.FModel+'FModel');
      Showmessage(LStudyFields.FStudyAreaName+'FStudyAreaName');
      Showmessage(LStudyFields.FSubArea+'FSubArea');
      Showmessage(LStudyFields.FScenario+'FScenario');
      Showmessage(LStudyFields.FScenarioLabel+'FScenarioLabel');
      Showmessage(LStudyFields.FScenarioDescr+'FScenarioDescr');
      Showmessage(LStudyFields.FDataFilesPrefix+'FDataFilesPrefix');
      Showmessage(LStudyFields.FDataFilesPath+'FDataFilesPath');
      Showmessage(booltostr(LStudyFields.FFilesLoaded)+'FFilesLoaded');
      Showmessage(booltostr(LStudyFields.FEditable)+'FEditable');
      Showmessage(inttostr(LStudyFields.FCalenderStartMonth)+'FCalenderStartMonth');
      Showmessage(LStudyFields.FVersion+'FVersion');
      }
      if not Assigned(HStudyList) then HStudyList := TStudyList.Create(HAppModules);
      PopulateStudyList(HStudyList,HAppModules);

      for LStudyIndex:=0 to HStudyList.StudyCount -1 do
      begin
        for LModelIndex:=0 to HStudyList[LStudyIndex].ModelCount -1 do
        begin
          for LSubAreaIndex:=0 to HStudyList[LStudyIndex].Model[LModelIndex].SubAreaCount -1 do
          begin
            for LScenarioIndex:=0 to HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].ScenarioCount -1 do
            begin
              if (HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].Scenario[LScenarioIndex].Scenario = LStudyFields.FScenario) and
                 (HStudyList[LStudyIndex].Model[LModelIndex].Model = LStudyFields.FModel) and
                 (HStudyList[LStudyIndex].Model[LModelIndex].SubArea[LSubAreaIndex].SubArea = LStudyFields.FSubArea) and
                 (HStudyList[LStudyIndex].Study = LStudyFields.FStudyAreaName) then
              begin
                     MessageDlg('Cannot insert this network here because it already exists',mtError,[mbOk],0);
                     FreeAndNil(LStudyFields);
                     exit;
              end;
            end;
          end;
        end;
      end;

      if AddStudy(LStudyFields,nlScenario) then
      begin
       if not Assigned(HStudyList) then HStudyList := TStudyList.Create(HAppModules);
        if PopulateStudyList(HStudyList, HAppModules) then
        begin
          SelectedNode := InsertTreeView.Selected.AbsoluteIndex;
          SetStudyList(HStudyList,InsertTreeView);
          ExpandTreeViewToAbsoluteIndex(InsertTreeView,SelectedNode);
          RefreshStudySelectionDialog;
        end;
      end
      else
      begin
        MessageDlg('The network could not be inserted.',mtError,[mbOk],0);
      end;
    finally
      FreeAndNil(LStudyFields);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.btnInsertClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.btnInsertClick';
var
  i : integer;
  LCode : String;
begin
  try
    for i:=0 to InsertListBox.Items.Count -1 do
    begin
      if InsertListBox.Selected[i] then
      begin
        LCode := InsertListBox.Items.Strings[i];
        break;
      end;
    end;

    AddNetworkToStudySubArea(LCode,InsertTreeView);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.TabSheet6Show(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.TabSheet6Show';
begin
  try
    //GetHydrologyModel;
    InsertListBox.Items.Delimiter := ',';
    InsertListBox.Items.DelimitedText := AllNetworkCodes;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.DeleteScenarioByNode(ANode : TTreeNode);
const OPNAME = 'TfrmHydroEditDialog.DeleteScenarioByNode';
var
  LStudyArea          : TStudyArea;
  LScenarioDataObject : TScenarioDataObject;
  LNode               : TTreeNode;
  LActionLevel        : TModelActionLevel;
  LResult             : boolean;
begin
  try
    LResult := false;
    LNode := ANode;
    LActionLevel := GetActionLevelFromNode(LNode);
    if (LActionLevel <> malNone) then
    begin
      LScenarioDataObject := GetScenarioDataObjectFromNode(LNode);
      if Assigned(LScenarioDataObject) then
      begin
        LStudyArea := TStudyArea.Create(HAppModules);
        try
          LStudyArea.CopyFromScenarioDataObject(LScenarioDataObject);
          case LActionLevel of
            malStudy:
            begin

              LResult := DeleteStudyData(HAppModules, malStudy, LStudyArea);
            end;
            malModel:
            begin
              LResult := DeleteStudyData(HAppModules, malModel, LStudyArea);
            end;
            malSubArea:
            begin
              LResult := DeleteStudyData(HAppModules, malSubArea, LStudyArea);
            end;
            malScenarion:
            begin
              LResult := DeleteStudyData(HAppModules, malScenarion, LStudyArea);
            end;
          end;
          if LResult then
          begin
            RefreshStudySelectionDialog;
            Sleep(500);
          end;
        finally
          LStudyArea.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.rdoDeleteNetworkClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.rdoDeleteNetworkClick';
begin
  try
    Panel5.Visible := true;
    Panel4.Visible := false;
    DeleteListBox.Items.Delimiter := ',';
    DeleteListBox.Items.DelimitedText := AllNetworkCodes;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.rdoDeleteStudyClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.rdoDeleteStudyClick';
begin
  try
    panel4.Visible := true;
    panel5.Visible := false;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.DeleteFromStudy;
const OPNAME = 'TfrmHydroEditDialog.DeleteFromStudy';
var
  LModelDataObject : TModelDataObject;
  LStudySelectionNodeData : TStudySelectionNodeData;
  LIndex : Integer;
  LScenarioDataObject: TScenarioDataObject;
  LNode: TTreeNode;
  LNetworkCode : WideString;
  LErrMsg : WideString;
  LNodesToDelete : Array of TTreeNode;
  SelectedNode : Integer;
begin
  try
    LModelDataObject := nil;
    LStudySelectionNodeData := TStudySelectionNodeData(DeleteTreeView.Selected.Data);
    // (nlStudy, nlModel, nlSubArea, nlScenario);
    case LStudySelectionNodeData.NodeLevel of
      nlStudy :    Begin
                  end;
      nlModel :    begin
                    LModelDataObject := TModelDataObject(DeleteTreeView.Selected.Data);
                   end;
      nlSubArea :  begin
                    LModelDataObject := TModelDataObject(DeleteTreeView.Selected.Parent.Data);
                  end;
      nlScenario : begin
                    LModelDataObject := TModelDataObject(DeleteTreeView.Selected.Parent.Parent.Data);
                  end;
    end;//case

    if Assigned(LModelDataObject) then
    begin
      if LModelDataObject.Model = 'Hydrology' then
      Begin
        LNode := DeleteTreeView.Selected;
        LScenarioDataObject := GetScenarioDataObjectFromNode(LNode);
        Setlength(LNodesToDelete,0);
        LNetworkCode := LScenarioDataObject.Scenario;

        if LNetworkCode = '' then
        begin
          MessageDlg('Please select a network to delete.',mtError,[mbOk],0);
          exit;
        end
        else
        begin
          if rdoDeleteFromStudy.Checked then
             if MessageDlg('Are you sure you want to delete the Network ['+LNetworkCode+'] from this study?',mtConfirmation,[mbYes,mbNo],0) = mrNo then exit;
             //                  if rdoDeleteAll.Checked then
             //                     if MessageDlg('Are you sure you want to delete all references to the network ['+LNetworkCode+']?',mtConfirmation,[mbYes,mbNo],0) = mrNo then exit;
        end;

        if rdoDeleteFromStudy.Checked then
        begin
          DeleteScenarioByNode(DeleteTreeView.Selected);
        end;

        if rdoDeleteAll.Checked then
        begin
          for LIndex:=0 to DeleteTreeView.Items.Count -1 do
          begin
            LNode := DeleteTreeView.Items[LIndex];
            LScenarioDataObject := GetScenarioDataObjectFromNode(LNode);
            LStudySelectionNodeData := TStudySelectionNodeData(LNode.Data);

            if (LScenarioDataObject.Scenario = LNetworkCode) and (LStudySelectionNodeData.NodeLevel = nlScenario) then
            begin
              //ShowMessage('Found - '+LNetworkCode+' -'+IntToStr(LIndex));
               SetLength(LNodesToDelete,length(LNodesToDelete)+1);
               LNodesToDelete[length(LNodesToDelete)-1] := LNode;
             end;
          end;

          LErrMsg := 'The following Scenarios will also be removed..';
          for LIndex:= 0 to length(LNodesToDelete) -1 do
          begin
             LNode := LNodesToDelete[LIndex];
             LScenarioDataObject := GetScenarioDataObjectFromNode(LNode);
             LErrMsg := LErrMsg +#13+ '    - ' + LScenarioDataObject.Scenario;
          end;

          if length(LNodesToDelete) = 0 then
            LErrMsg := '  - No Scenarios are currently linked to this network..';
          if MessageDlg('Are you sure you want to delete all references to the network ['+LNetworkCode+']?'+#13+#13+
             LErrMsg,mtConfirmation,[mbYes,mbNo],0) = mrNo then
             exit;

          for LIndex := 0 to length(LNodesToDelete) -1 do
          begin
             DeleteScenarioByNode(LNodesToDelete[LIndex]);
          end;

          if not DeleteNetwork(LNetworkCode,LErrMsg) then
          begin
            MessageDlg('The network ['+LNetworkCode+'] could not be deleted.'+#13+#13+LErrMsg,mtError,[mbOk],0);
            if not Assigned(HStudyList) then HStudyList := TStudyList.Create(HAppModules);
          end
          else
          begin
            if PopulateStudyList(HStudyList, HAppModules) then
            begin
              SelectedNode := DeleteTreeView.Selected.AbsoluteIndex;
              SetStudyList(HStudyList,DeleteTreeView);
              ExpandTreeViewToAbsoluteIndex(DeleteTreeView,SelectedNode);
              SetStudyList(HStudyList,CopyTreeView);
              ExpandTreeViewToAbsoluteIndex(CopyTreeView,SelectedNode);
              RefreshStudySelectionDialog;
            end;
          end;
        end;
      end
      else
      begin
        MessageDlg('Unfortunately you can only delete Hydrology Scenarios from here',mtInformation,[mbOk],0);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.btnDeleteClick(Sender: TObject);
const OPNAME = 'TfrmHydroEditDialog.btnDeleteClick';
begin
  try
    if rdoDeleteStudy.Checked then
    begin
      DeleteFromStudy;
    end;

    if rdoDeleteNetwork.Checked then
    begin
      DeleteFromNetwork;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.DeleteFromNetwork;
const OPNAME = 'TfrmHydroEditDialog.DeleteFromNetwork';
var
  LStudySelectionNodeData : TStudySelectionNodeData;
  LIndex : Integer;
  LScenarioDataObject: TScenarioDataObject;
  LNode: TTreeNode;
  LNetworkCode : WideString;
  LErrMsg : WideString;
  LNodesToDelete : Array of TTreeNode;
begin
  try
    LNodesToDelete := nil;
    try
      for LIndex := 0 to DeleteListBox.Items.Count -1 do
      begin
        if DeleteListBox.Selected[LIndex] then
          LNetworkCode := DeleteListBox.Items.Strings[LIndex];
      end;

      if LNetworkCode = '' then
      begin
        MessageDlg('Please select a network to delete.',mtError,[mbOk],0);
        Exit;
      end;

      for LIndex:=0 to DeleteTreeView.Items.Count -1 do
      begin
        LNode := DeleteTreeView.Items[LIndex];
        LScenarioDataObject := GetScenarioDataObjectFromNode(LNode);
        LStudySelectionNodeData := TStudySelectionNodeData(LNode.Data);

        if (LScenarioDataObject.Scenario = LNetworkCode) and (LStudySelectionNodeData.NodeLevel = nlScenario) then
        begin
          //ShowMessage('Found - '+LNetworkCode+' -'+IntToStr(LIndex));
          SetLength(LNodesToDelete,length(LNodesToDelete)+1);
          LNodesToDelete[length(LNodesToDelete)-1] := LNode;
        end;
      end;

      LErrMsg := 'The following Scenarios will also be removed..';
      for LIndex:= 0 to length(LNodesToDelete) -1 do
       begin
         LNode := LNodesToDelete[LIndex];
         LScenarioDataObject := GetScenarioDataObjectFromNode(LNode);
         LErrMsg := LErrMsg +#13+ '    - ' + LScenarioDataObject.Scenario;
       end;

       if length(LNodesToDelete) = 0 then
         LErrMsg := '  - No Scenarios are currently linked to this network..';
       if MessageDlg('Are you sure you want to delete all references to the network ['+LNetworkCode+']?'+#13+#13+
                     LErrMsg,mtConfirmation,[mbYes,mbNo],0) = mrNo then
         exit;

       for LIndex := 0 to length(LNodesToDelete) -1 do
       begin
         DeleteScenarioByNode(LNodesToDelete[LIndex]);
       end;

       if not DeleteNetwork(LNetworkCode,LErrMsg) then
         MessageDlg('The network ['+LNetworkCode+'] could not be deleted.'+#13+#13+LErrMsg,mtError,[mbOk],0)
       else
       begin
         DeleteListBox.Items.Delimiter := ',';
         DeleteListBox.Items.DelimitedText := AllNetworkCodes;
       end;
     finally
       Finalize(LNodesToDelete);
     end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.AllNetworkCodes: WideString;
const OPNAME = 'TfrmHydroEditDialog.AllNetworkCodes';
begin
  Result := '';
  try
    if not Assigned(FHydroDBManager) then
      FHydroDBManager := THydroDBManager.Create;
    Result := FHydroDBManager.AllNetworkCodes;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.CreateNewNetwork(const ANetworkCode: WideString; AVersionNo: Integer;
  const AInputDir, AOutputDir, ADebugRequired: WideString; ADebugStartPeriod, ADebudEndPeriod: Integer;
  const ASummaryRequired: WideString; ASimulationStartYear, ASimulationEndYear, AReadOnly: Integer;
  var ANetworkID: Integer; var AErrorMsg: WideString): WordBool;
const OPNAME = 'TfrmHydroEditDialog.CreateNewNetwork';
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    Result := FHydroDBManager.CreateNewNetwork(ANetworkCode, AVersionNo, AInputDir, AOutputDir, ADebugRequired,
              ADebugStartPeriod, ADebudEndPeriod, ASummaryRequired, ASimulationStartYear, ASimulationEndYear,
              AReadOnly, ANetworkID, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.DeleteNetwork(const ANetworkCode: WideString; var AErrorMsg: WideString): WordBool;
const OPNAME = 'TfrmHydroEditDialog.DeleteNetwork';
var
  LDiagramDir : String;
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    LDiagramDir := GetAppDataLocalDir+'\Network Diagrams\'+      //ExtractFilePath(ApplicationExeName) + 'Network Diagrams\'+
                   ChopCharacters(HAppModules.StudyArea.StudyAreaCode) + '\' +
                   ChopCharacters(HAppModules.StudyArea.ScenarioCode) + '\';
    Result := FHydroDBManager.DeleteNetwork(ANetworkCode, LDiagramDir, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.CopyNetwork(const AOldNetworkCode,ANewNetworkCode: WideString; var AErrorMsg: WideString): WordBool;
const OPNAME = 'TfrmHydroEditDialog.CopyNetwork';
var
  LDiagramDir : String;
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    LDiagramDir := GetAppDataLocalDir+'\Network Diagrams\'+  //ExtractFilePath(ApplicationExeName) + 'Network Diagrams\'+
                   ChopCharacters(HAppModules.StudyArea.StudyAreaCode) + '\' +
                   ChopCharacters(HAppModules.StudyArea.ScenarioCode) + '\';
    Result := FHydroDBManager.CopyNetwork(AOldNetworkCode, ANewNetworkCode, LDiagramDir, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.ExportNetwork(const ANetworkCode, ADirectory: WideString; var AErrorMsg: WideString): WordBool;
const OPNAME = 'TfrmHydroEditDialog.ExportNetwork';
var
  LDiagramDir : String;
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    LDiagramDir := GetAppDataLocalDir+'\Network Diagrams\'+ //ExtractFilePath(ApplicationExeName) + 'Network Diagrams\'+
                   ChopCharacters(HAppModules.StudyArea.StudyAreaCode) + '\' +
                   ChopCharacters(HAppModules.StudyArea.ScenarioCode) + '\';
    Result := FHydroDBManager.ExportNetwork(ANetworkCode, ADirectory, LDiagramDir, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmHydroEditDialog.ImportNetwork(const ADirectory: WideString; var ANetworkCode, AErrorMsg: WideString): WordBool;
const OPNAME = 'TfrmHydroEditDialog.ImportNetwork';
var
  LDiagramDir : String;
begin
  Result := FALSE;
  try
    AErrorMsg := '';
    LDiagramDir := GetAppDataLocalDir+'\Network Diagrams\'+ //ExtractFilePath(ApplicationExeName) + 'Network Diagrams\'+
                   ChopCharacters(HAppModules.StudyArea.StudyAreaCode) + '\';
    Result := FHydroDBManager.ImportNetwork(ADirectory, LDiagramDir, ANetworkCode, AErrorMsg);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmHydroEditDialog.GetNetworkList;
const OPNAME = 'TfrmHydroEditDialog.ImportNetwork';
begin
  try
    if not Assigned(HNetworkList) then
      HNetworkList := TStringList.create;
    HNetworkList.Delimiter := ',';
    HNetWorkList.DelimitedText := AllNetworkCodes;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
