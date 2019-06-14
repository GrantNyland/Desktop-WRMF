//
//
//  UNIT      : Contains the class TLaneGUITesterMainForm.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/05
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UStudyAreaSelectionDialog;


interface


{$R *.DFM}


//
// Interface dependencies
//
uses

  // Delphi
  DB,
  Classes,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Forms,
  Windows,
  Vcl.Menus,

  // DWAF
  //UHTMLHelp,
  //D6OnHelpFix,

  UUtilities,
  UStudyList,
  UStudyObjects,
  UAbstractObject,
  UAbstractComponent,
  UStudyDetailsPanel,
  UAbstractModelObjects,
  USystemModelLinkClasses,
  UStudySelectionTreeView,
  UGISStudyAreaSelectorPanel,
  UStudySelectionButtonPanel;


//
// This dialog is used to select a study area.
//
type
  TStudyAreaSelectionDialog = class(TAbstractForm)
    FSplitter: TSplitter;
    FBackPanel: TPanel;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuImportStudy: TMenuItem;
    mnuExporStudy: TMenuItem;
    mnuCopyStudy: TMenuItem;
    mnuDeleteStudy: TMenuItem;
    mnuExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditStudy: TMenuItem;
    mnuSelectStudy: TMenuItem;
    mnuNewStudy: TMenuItem;
    mnuReports: TMenuItem;
    mnuViewStudyReports: TMenuItem;
    mnuBuildDatabase: TMenuItem;
    mnuExportSystemData: TMenuItem;
    mnuUnlockScenario: TMenuItem;
    mnuSeparator1: TMenuItem;
    mnuSeparator2: TMenuItem;
    Edit1: TMenuItem;
    mnuCopyGISToClibboard: TMenuItem;
    mnuExportGISToFile: TMenuItem;
    mnuPrintGIS: TMenuItem;
    mnuImportAll: TMenuItem;
    mnuExportAll: TMenuItem;
    mnuLinkHydrologyStudies: TMenuItem;
    mnuUnLinkHydrologyStudies: TMenuItem;
    procedure mnuExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,NewHeight: Integer; var Resize: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    FStudyTreeView: TStudySelectionTreeView;
    FStudyDetailsPanel: TStudyDetailsPanel;
    FGISStudyAreaSelectorPanel: TGISStudyAreaSelectorPanel;
    FButtonPanel: TStudySelectionButtonPanel;
    FStudyList: TStudyList;
    FStudyDetailsSplitter: TSplitter;
    FPreSelectedNode: TTreeNode;
    FIgnoreNodeChange: boolean;
    FPreviousStudy      : string;
    FFullExtentLeft     : double;
    FFullExtentRight    : double;
    FFullExtentTop      : double;
    FFullExtentBottom   : double;
    procedure CreateMemberObjects; override;
    procedure DoGISStudyAreaSelectionEvent(AModelName, ASubAreaName: string; ADateLaterThan: TDateTime; AActionRequired: TStudySelectionAction);
    procedure SelectNodeBySubArea(AModelName, ASubAreaName: string; ADateLaterThan: TDateTime);
    procedure DoTreeViewChange(ASender: TObject; ANode: TTreeNode);
    procedure DoStudySelectionClick(Sender : TObject);
    procedure DoCancelButtonClick ( Sender : TObject );
    procedure SetStudyDetailsPanelStates;
    procedure SetGISPanelState;
    procedure SetGISExtent;
    function CurrentNodeIsSelectedStudy: boolean;
    function CurrentNodeIsScenarioWithNoData: boolean;
    function CurrentNodeIsRainfallProjectGauges: boolean;
    procedure AssignHelpContext; override;
    //function OnHelpRequest(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
    function GetGISStudyAreaSelectorPanel : TGISStudyAreaSelectorPanel;
  public
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;

    function GetSelectedNode(AStudy, AModel, ASubArea, AScenario: string): TTreeNode;
    function PopulateFieldsFromSelectionDialog(var ANodeLevel: TNodeLevel; var AStudyFields: TStudyFields): boolean;
    function GetModelNamesForSelectedNode(AModelNames: TStrings): boolean;
    function GetScenarioDataObjectFromNode(ANode: TTreeNode):TScenarioDataObject;

    procedure SetButtonStates;
    procedure SelectedNode(AStudy, AModel, ASubArea, AScenario: string);
    procedure SetExpandedNodes(AList : TStrings);
    procedure SetStudyList(AStudyList: TStudyList);

    property StudyTreeView: TStudySelectionTreeView read FStudyTreeView;
    property PreSelectedNode: TTreeNode read FPreSelectedNode write FPreSelectedNode;
    property IgnoreNodeChange: boolean read FIgnoreNodeChange write FIgnoreNodeChange;
    property StudyDetailsPanel: TStudyDetailsPanel read FStudyDetailsPanel;
    property GISStudyAreaSelectorPanel : TGISStudyAreaSelectorPanel read GetGISStudyAreaSelectorPanel;
    property ButtonPanel: TStudySelectionButtonPanel read FButtonPanel;
  end;

implementation

uses
  SysUtils,
  UConstants,
  UDataSetType,
  UHelpContexts,
  UStringListOfStringLists,
  UErrorHandlingOperations;

const
  C_DefaultTreeViewWidth = 100;
  C_DefaultPanelHeight   = 100;
  C_IdSplitterPos        = 'SplitterPos';
  C_IdPanelHeight        = 'PanelHeight';

procedure TStudyAreaSelectionDialog.AssignHelpContext;
const OPNAME = 'TStudyAreaSelectionDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,           HC_WaterResourcesYieldModel);
    SetControlHelpContext(FBackPanel,     HC_WaterResourcesYieldModel);
    SetControlHelpContext(FStudyTreeView, HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end; 

procedure TStudyAreaSelectionDialog.CreateMemberObjects;
const OPNAME = 'TStudyAreaSelectionDialog.CreateMemberObjects';
begin
  try
    FIgnoreNodeChange := False;
    FPreSelectedNode := nil;

    FFullExtentLeft   := 14.7275;
    FFullExtentRight  := -19.7953;
    FFullExtentTop    := 35.1797;
    FFullExtentBottom := -34.7693;

    // Create the button panel.
    FButtonPanel := TStudySelectionButtonPanel.Create(self, FAppModules);
    FButtonPanel.Parent := self;
    FButtonPanel.Align  := alTop;
    FButtonPanel.OKButton.OnClick := DoStudySelectionClick;
    FButtonPanel.CancelButton.OnClick := DoCancelButtonClick;
    FButtonPanel.SetButtonStates(False, False, False,False,False, nil);

    mnuSelectStudy.OnClick      := DoStudySelectionClick;
    mnuViewStudyReports.OnClick := FButtonPanel.LaunchReport;

    // Create the tree view.
    FStudyTreeView := TStudySelectionTreeView.Create(self, FAppModules);
    FStudyTreeView.Parent := self;
    FStudyTreeView.Align := alLeft;
    FStudyTreeView.ReadOnly := True;
    FStudyTreeView.HideSelection := False;
    FStudyTreeView.OnChange := DoTreeViewChange;
    FStudyTreeView.OnDblClick := DoStudySelectionClick;

    // Create the study details panel.
    FStudyDetailsPanel := TStudyDetailsPanel.Create(FBackPanel, FAppModules);
    FStudyDetailsPanel.Parent := FBackPanel;
    FStudyDetailsPanel.Align := alTop;
    FStudyDetailsPanel.Height := FBackPanel.Height div 4;

    // Create the study details panel splitter.
    FStudyDetailsSplitter        :=  TSplitter.Create(FBackPanel);
    FStudyDetailsSplitter.Beveled := False;
    FStudyDetailsSplitter.Parent := FBackPanel;
    FStudyDetailsSplitter.Align  := alTop;
    FStudyDetailsSplitter.Top := FStudyDetailsPanel.Height + 1;

    // Create the GIS panel.
    FGISStudyAreaSelectorPanel := TGISStudyAreaSelectorPanel.Create(FBackPanel, FAppModules);
    FGISStudyAreaSelectorPanel.Parent := FBackPanel;
    FGISStudyAreaSelectorPanel.Align := alClient;
    FGISStudyAreaSelectorPanel.OnStudyAreaSelectionEvent := DoGISStudyAreaSelectionEvent;

    FStudyList := nil;
    //OnHelp := OnHelpRequest;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.FormShow(Sender: TObject);
const OPNAME = 'TStudyAreaSelectionDialog.FormShow';
begin
  try
    // Get the objects last view settings. Discard any errors here because they are not important.
    try
      FStudyTreeView.Width := FAppModules.ViewIni.ReadInteger(ClassName, C_IdSplitterPos, C_DefaultTreeViewWidth);
      FStudyDetailsPanel.Height := FAppModules.ViewIni.ReadInteger(ClassName, C_IdPanelHeight, C_DefaultPanelHeight);
    except end;

    if(FPreSelectedNode <> nil) then
      FStudyTreeView.Selected := FPreSelectedNode;
    SetButtonStates;
  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.FormClose(Sender: TObject;var Action: TCloseAction);
const OPNAME = 'TStudyAreaSelectionDialog.CloseQuery';
begin
  try
    // Save the screen settings. Discard any errors here because they are not important.
    try
      FAppModules.ViewIni.WriteInteger(ClassName, C_IdSplitterPos, FStudyTreeView.Width);
      FAppModules.ViewIni.WriteInteger(ClassName, C_IdPanelHeight, FStudyDetailsPanel.Height);
    except end;

  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.SetStudyList(AStudyList: TStudyList);
const OPNAME = 'TStudyAreaSelectionDialog.SetStudyList';
var
  LStudyNode: TTreeNode;
begin
  try
    FStudyTreeView.OnChange := nil;
    FStudyTreeView.Selected := nil;
    FStudyList := AStudyList;

    try
      FStudyTreeView.SetStudyList(AStudyList);
    finally
      FStudyTreeView.OnChange := DoTreeViewChange;
    end;
    if (FStudyTreeView.Items.Count > 0) then
    begin
      LStudyNode     := FStudyTreeView.Items[0];
      if(LStudyNode <> nil) then
        FStudyTreeView.Selected := LStudyNode;
    end;
    Self.ActiveControl := FStudyTreeView;

    AStudyList.LoadAvailableModelsInto(FGISStudyAreaSelectorPanel.ModelCombo.Items);
    try
      FGISStudyAreaSelectorPanel.ModelCombo.ItemIndex :=
        FGISStudyAreaSelectorPanel.ModelCombo.Items.IndexOf(
          FAppModules.ViewIni.ReadString(FGISStudyAreaSelectorPanel.ClassName, C_IdLastModel, ''));
    except end;
    
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.DoTreeViewChange(ASender: TObject; ANode: TTreeNode);
const OPNAME = 'TStudyAreaSelectionDialog.DoTreeViewChange';
begin
  try
    if FIgnoreNodeChange then Exit;
    SetStudyDetailsPanelStates;
    SetGISPanelState;
    SetGISExtent;
    SetButtonStates;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.DoStudySelectionClick(Sender: TObject);
const OPNAME = 'TStudyAreaSelectionDialog.DoStudySelectionClick';
begin
  try
    ModalResult := mrNone;
    if (Assigned(FStudyTreeView.Selected) and
        Assigned(FStudyTreeView.Selected.Data)) then
      if (FStudyTreeView.PopulateStudyArea(FStudyTreeView.Selected)) then
        ModalResult := mrOK;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.DoGISStudyAreaSelectionEvent(
  AModelName, ASubAreaName: string; ADateLaterThan: TDateTime; AActionRequired: TStudySelectionAction);
const OPNAME = 'TStudyAreaSelectionDialog.DoGISStudyAreaSelectionEvent';
begin
  try
    case AActionRequired of
      ssaNoStudy :
        begin
          if (not Assigned(FStudyTreeView.Selected)) then
          begin
            FStudyTreeView.Selected := nil;
            DoTreeViewChange(nil, nil);
          end;
        end;
      ssaShowStudyDetails :
        begin
          SelectNodeBySubArea(AModelName, ASubAreaName, ADateLaterThan);
        end;
      ssaDoStudySelection :
        begin
          SelectNodeBySubArea(AModelName, ASubAreaName, ADateLaterThan);
          DoStudySelectionClick(nil);
        end;
    else
      raise Exception.CreateFmt('Unknown GIS study selection action [%d].', [integer(AActionRequired)]);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.SelectNodeBySubArea(
  AModelName, ASubAreaName: string; ADateLaterThan: TDateTime);
const OPNAME = 'TStudyAreaSelectionDialog.SelectNodeBySubArea';
var LNewNode: TTreeNode;
begin
  try

    // Find the new node.
    LNewNode := FStudyTreeView.FindFirstSubAreaNodeAfter(AModelName, ASubAreaName, ADateLaterThan);
    if (LNewNode <> FStudyTreeView.Selected) then
    begin
      if (FStudyTreeView.Selected = nil) or (FStudyTreeView.Selected.Level < 3) then
      begin
        FStudyTreeView.Selected := LNewNode;
        DoTreeViewChange(nil, nil);
      end else begin
        if (LNewNode <> FStudyTreeView.Selected.Parent) then
        begin
          FStudyTreeView.Selected := LNewNode;
          DoTreeViewChange(nil, nil);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyAreaSelectionDialog.LanguageHasChanged: boolean;
const OPNAME = 'TStudyAreaSelectionDialog.LanguageHasChanged';
begin
  Result := False;
  try
    Result := FStudyTreeView.LanguageHasChanged and
              FStudyDetailsPanel.LanguageHasChanged and
              FButtonPanel.LanguageHasChanged and
              FGISStudyAreaSelectorPanel.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyAreaSelectionDialog.Initialise: boolean;
const OPNAME = 'TStudyAreaSelectionDialog.Initialise';
begin
  Result := False;
  try
    Result := FStudyTreeView.Initialise and
              FStudyDetailsPanel.Initialise and
              FButtonPanel.Initialise and
              FGISStudyAreaSelectorPanel.Initialise;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.SetButtonStates;
const OPNAME = 'TStudyAreaSelectionDialog.SetButtonStates';
var
  LProjectGauges,
  LEditable           : boolean;
  LDeletableNode      : boolean;
  lCanCopy            : boolean;
  LSysAdminUser       : boolean;
  LUpdateUser         : boolean;
  LScenarioDataObject : TScenarioDataObject;
  LModelCode          : string;
  LStudyCode          : string;
  LSubArea            : string;
  LScenarioCode       : string;
  LAppModelCode       : string;
  LAppStudyCode       : string;
  LAppSubArea         : string;
  LAppScenarioCode    : string;
begin
  try
    LUpdateUser := Assigned(FAppModules) and
                   (FAppModules.User <> nil) and
                   (FAppModules.User.UserRights in CUR_EditData);
    if Assigned(FStudyTreeView.Selected) and
       Assigned(FStudyTreeView.Selected.Data) then
    begin
      LDeletableNode := LUpdateUser AND
                        (NOT CurrentNodeIsSelectedStudy) AND
                        ((FStudyTreeView.Selected.Level = 0) OR
                         FStudyTreeView.MoreThanOneNodeAtLevel(FStudyTreeView.Selected));
      LEditable      := TStudySelectionNodeData(FStudyTreeView.Selected.Data).Editable;
      LEditable      := LEditable and LUpdateUser;
      lCanCopy       := LEditable;
      LDeletableNode := LDeletableNode and LEditable;
      LProjectGauges := False;
      if CurrentNodeIsRainfallProjectGauges then
      begin
        LDeletableNode := False;
        lCanCopy       := False;
        LProjectGauges := True;
      end;

      case TStudySelectionNodeData(FStudyTreeView.Selected.Data).NodeLevel of
        nlStudy   : FButtonPanel.SetButtonStates(True  ,LEditable  ,LEditable ,LDeletableNode ,lCanCopy  ,TStudySelectionNodeData(FStudyTreeView.Selected.Data).DocumentDetail.DocumentDetail['StudyDescription']);
        nlModel   : FButtonPanel.SetButtonStates(True  ,False      ,LEditable ,LDeletableNode ,False     ,TStudySelectionNodeData(FStudyTreeView.Selected.Data).DocumentDetail.DocumentDetail['ModelDescription']);
        nlSubArea : FButtonPanel.SetButtonStates(True  ,LEditable  ,LEditable ,LDeletableNode ,lCanCopy  ,TStudySelectionNodeData(FStudyTreeView.Selected.Data).DocumentDetail.DocumentDetail['SubAreaDescription']);
        nlScenario:
        begin
          if LProjectGauges then
            FButtonPanel.SetButtonStates(True,  LEditable,  False, LDeletableNode, lCanCopy, TStudySelectionNodeData(FStudyTreeView.Selected.Data).DocumentDetail.DocumentDetail['ScenarioDescription'])
          else
            FButtonPanel.SetButtonStates(True,  LEditable,  LEditable, LDeletableNode, lCanCopy, TStudySelectionNodeData(FStudyTreeView.Selected.Data).DocumentDetail.DocumentDetail['ScenarioDescription']);
        end;
      else
        FButtonPanel.SetButtonStates(False, False, False,False,False, nil);
      end;
    end
    else
    begin
      FButtonPanel.SetButtonStates(False, False, (FStudyTreeView.Items.Count = 0), False, False, nil);
    end;
    LSysAdminUser := Assigned(FAppModules) and
                     (FAppModules.User <> nil) and
                     (FAppModules.User.UserRights = CUR_SysAdmin);
    mnuImportStudy.Enabled      := LUpdateUser OR
                                   LSysAdminUser;

    mnuImportAll.Enabled        := LUpdateUser OR
                                   LSysAdminUser;
    mnuExportAll.Enabled        := (LUpdateUser or
                                     LSysAdminUser) and (FStudyTreeView.Selected <> nil);


    mnuExporStudy.Enabled       := FButtonPanel.OKButton.Enabled AND
                                   (FStudyTreeView.Selected <> nil) {AND
                                   (FStudyTreeView.Selected.Level >= 2)};
    mnuBuildDatabase.Enabled    := LSysAdminUser;
    mnuExportSystemData.Enabled := LSysAdminUser;
    mnuCopyStudy.Enabled        := LUpdateUser and FButtonPanel.CopyButton.Enabled;
    mnuDeleteStudy.Enabled      := LUpdateUser and FButtonPanel.DeleteButton.Enabled;
    mnuEditStudy.Enabled        := LUpdateUser and FButtonPanel.EditButton.Enabled;
    mnuSelectStudy.Enabled      := FButtonPanel.OKButton.Enabled;
    mnuNewStudy.Enabled         := LUpdateUser and FButtonPanel.NewButton.Enabled;
    mnuViewStudyReports.Enabled := FButtonPanel.ReportButton.Enabled;

    mnuUnlockScenario.Enabled         := False;
    mnuLinkHydrologyStudies.Enabled   := False;
    mnuUnLinkHydrologyStudies.Enabled := False;
    mnuExportGISToFile.Enabled        := False;
    if (FStudyTreeView.Selected <> nil) AND (TModelDataObject(FStudyTreeView.Selected.Data).NodeLevel = nlScenario) then
    begin
      LScenarioDataObject         := TScenarioDataObject(FStudyTreeView.Selected.Data);

      if LUpdateUser and Assigned(LScenarioDataObject) then
      begin
        LModelCode                  := LScenarioDataObject.SubArea.Model.Model;
        LStudyCode                  := LScenarioDataObject.SubArea.Model.Study.Study;
        LSubArea                    := LScenarioDataObject.SubArea.SubArea;
        LScenarioCode               := LScenarioDataObject.Scenario;

        mnuLinkHydrologyStudies.Enabled    := (LModelCode = CHydrology);
        mnuUnLinkHydrologyStudies.Enabled  := (LModelCode = CHydrology);
        mnuExportGISToFile.Enabled         := True;

        if(FAppModules.StudyArea <> nil) then
        begin
          LAppModelCode               := FAppModules.StudyArea.ModelCode;
          LAppStudyCode               := FAppModules.StudyArea.StudyAreaCode;
          LAppSubArea                 := FAppModules.StudyArea.SubAreaCode;
          LAppScenarioCode            := FAppModules.StudyArea.ScenarioCode;
        end
        else
        begin
          LAppModelCode               := '';
          LAppStudyCode               := '';
          LAppSubArea                 := '';
          LAppScenarioCode            := '';
        end;
        if (Trim(LModelCode)    <> Trim(LAppModelCode))    or
           (Trim(LStudyCode)    <> Trim(LAppStudyCode))    or
           (Trim(LSubArea)      <> Trim(LAppSubArea))      or
           (Trim(LScenarioCode) <> Trim(LAppScenarioCode)) then
          mnuUnlockScenario.Enabled := FAppModules.ScenarioLockManager.RequestLockStatus(LModelCode,
                                                                                         LStudyCode,
                                                                                         LSubArea,
                                                                                         LScenarioCode);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.SetStudyDetailsPanelStates;
const OPNAME = 'TStudyAreaSelectionDialog.SetStudyDetailsPanelStates';
begin
  try
    if Assigned(FStudyTreeView.Selected) and
       Assigned(FStudyTreeView.Selected.Data) then
    begin
      case TStudySelectionNodeData(FStudyTreeView.Selected.Data).NodeLevel of
         nlStudy    : FStudyDetailsPanel.ShowStudy(TStudyDataObject(FStudyTreeView.Selected.Data));
         nlModel    : FStudyDetailsPanel.ShowModel(TModelDataObject(FStudyTreeView.Selected.Data));
         nlSubArea  : FStudyDetailsPanel.ShowSubArea(TSubAreaDataObject(FStudyTreeView.Selected.Data));
         nlScenario : FStudyDetailsPanel.ShowScenario(TScenarioDataObject(FStudyTreeView.Selected.Data));
      else
        FStudyDetailsPanel.ShowNothing;
      end;
    end else begin
      FStudyDetailsPanel.ShowNothing;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.SetGISExtent;
const OPNAME = 'TStudyAreaSelectionDialog.SetGISExtent';
var
  LScenarioData : TScenarioDataObject;
begin
  try
    if Assigned(FStudyTreeView.Selected) and
       Assigned(FStudyTreeView.Selected.Data) then
    begin
      LScenarioData := GetScenarioDataObjectFromNode(FStudyTreeView.Selected);
      if(LScenarioData <> nil) then
      begin
        if(FPreviousStudy = '') then
          FPreviousStudy := LScenarioData.SubArea.Model.Study.Study
        else
        if(LScenarioData.SubArea.Model.Study.Study <> FPreviousStudy) then
        begin
          {FGISStudyAreaSelectorPanel.GISViewer.Extent_Left   := 14.4759059075627;
          FGISStudyAreaSelectorPanel.GISViewer.Extent_Right  := 32.2298897277899;
          FGISStudyAreaSelectorPanel.GISViewer.Extent_Top    := -22.1089071971839;
          FGISStudyAreaSelectorPanel.GISViewer.Extent_Bottom := -34.4117744500066;
          FGISStudyAreaSelectorPanel.GISViewer.ControlInterface.Control_RefreshAll;
          }
          FPreviousStudy := LScenarioData.SubArea.Model.Study.Study;
        end;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.SetGISPanelState;
const OPNAME = 'TStudyAreaSelectionDialog.SetGISPanelState';
var
  LPath,
  LMainPath,
  LStudy,
  LSubArea,
  LStudyShapefile,
  LSubAreaShapeFile: string;
  LActionLevel: TModelActionLevel;
begin
  try
    FGISStudyAreaSelectorPanel.BlockEvents := True;
    try
      if Assigned(FStudyTreeView.Selected) and
         Assigned(FStudyTreeView.Selected.Data) then
      begin

        LActionLevel      := malNone;
        LStudy            := '';
        LSubArea          := '';
        LStudyShapefile   := '';
        LSubAreaShapeFile := '';
        LMainPath         := GISCoversDirectory;
        case TStudySelectionNodeData(FStudyTreeView.Selected.Data).NodeLevel of
          nlStudy    :
            begin
              LActionLevel      := malStudy;
              LStudy            := TStudyDataObject( FStudyTreeView.Selected.Data ).Study;
              LSubArea          := LStudy;
              LPath             := LMainPath + 'Study Area\';
              LStudyShapefile   := LPath + TStudyDataObject( FStudyTreeView.Selected.Data ).StudyShapeFileName;
              LSubAreaShapeFile := '';
            end;

          nlModel    :
            begin
              LActionLevel      := malStudy;
              LStudy            := TModelDataObject(FStudyTreeView.Selected.Data).Study.Study;
              LSubArea          := '';
              LPath             := LMainPath + 'Study Area\';
              LStudyShapefile   := LPath + TModelDataObject(FStudyTreeView.Selected.Data).Study.StudyShapeFileName;
              LSubAreaShapeFile := '';
            end;

          nlSubArea  :
            begin
              LActionLevel      := malSubArea;
              LStudy            := TSubAreaDataObject(FStudyTreeView.Selected.Data).Model.Study.Study;
              LSubArea          := UpperCase(TSubAreaDataObject(FStudyTreeView.Selected.Data).SubArea);
              LPath             := LMainPath + 'Study Area\';
              LStudyShapefile   := LPath + TSubAreaDataObject(FStudyTreeView.Selected.Data).Model.Study.StudyShapeFileName;
              LPath             := LMainPath + 'Study Sub-Areas\';
              LSubAreaShapeFile := LPath + TSubAreaDataObject(FStudyTreeView.Selected.Data).SubAreaShapeFileName;
            end;
          nlScenario :
            begin
              LActionLevel      := malScenarion;
              LStudy            := TScenarioDataObject(FStudyTreeView.Selected.Data).SubArea.Model.Study.Study;
              LSubArea          := UpperCase(TScenarioDataObject(FStudyTreeView.Selected.Data).SubArea.SubArea);
              LPath             := LMainPath + 'Study Area\';
              LStudyShapefile   := LPath + TScenarioDataObject( FStudyTreeView.Selected.Data ).SubArea.Model.Study.StudyShapeFileName;
              LPath             := LMainPath + 'Study Sub-Areas\';
              LSubAreaShapeFile := LPath + TScenarioDataObject(FStudyTreeView.Selected.Data).SubArea.SubAreaShapeFileName;
            end;
        end;//Case
        FGISStudyAreaSelectorPanel.SetSelection(LStudy, LSubArea, LStudyShapefile, LSubAreaShapeFile, LActionLevel);
      end
      else
        FGISStudyAreaSelectorPanel.SetSelection('', '','','',malNone);
    finally
      FGISStudyAreaSelectorPanel.BlockEvents := False;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyAreaSelectionDialog.GetModelNamesForSelectedNode(AModelNames: TStrings): boolean;
const OPNAME = 'TStudyAreaSelectionDialog.GetModelNamesForSelectedNode';
var
  LStudySelectionNodeData: TStudySelectionNodeData;
  LModelDataObject:  TModelDataObject;
  LNodeLevel:TNodeLevel;
  LNode: TTreeNode;
  LIndex: integer;
begin
  Result := False;
  try
    if not Assigned(AModelNames) then
       raise Exception.Create('Model names parameter is not yet assigned.');
    AModelNames.Clear;
    if Assigned(FStudyTreeView.Selected) and Assigned(FStudyTreeView.Selected.Data) then
    begin
      LStudySelectionNodeData := TStudySelectionNodeData(FStudyTreeView.Selected.Data);
      LNodeLevel              := LStudySelectionNodeData.NodeLevel;
      LNode                   := nil;
      case LNodeLevel of
        nlStudy:     LNode := FStudyTreeView.Selected;
        nlModel:     LNode := FStudyTreeView.Selected.Parent;
        nlSubArea:   LNode := FStudyTreeView.Selected.Parent.Parent;
        nlScenario:  LNode := FStudyTreeView.Selected.Parent.Parent.Parent;
      end;//case

      for LIndex := 0 to LNode.Count -1 do
      begin
        if(LNode.Item[LIndex].Data <> nil) then
        begin
          LModelDataObject :=  TModelDataObject(LNode.Item[LIndex].Data);
          AModelNames.Add(LModelDataObject.Model);
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyAreaSelectionDialog.PopulateFieldsFromSelectionDialog(var ANodeLevel: TNodeLevel; var AStudyFields: TStudyFields): boolean;
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
    if Assigned(FStudyTreeView.Selected) and
       Assigned(FStudyTreeView.Selected.Data) then
    begin
      LStudySelectionNodeData := TStudySelectionNodeData(FStudyTreeView.Selected.Data);
      ANodeLevel := LStudySelectionNodeData.NodeLevel;
      case ANodeLevel of
        nlStudy:
          begin
            LStudyDataObject    := TStudyDataObject(FStudyTreeView.Selected.Data);
          end;
        nlModel:
          begin
            LStudyDataObject := TStudyDataObject(FStudyTreeView.Selected.Parent.Data);
            LModelDataObject :=  TModelDataObject(FStudyTreeView.Selected.Data);
          end;
        nlSubArea:
          begin
            LStudyDataObject := TStudyDataObject(FStudyTreeView.Selected.Parent.Parent.Data);
            LModelDataObject :=  TModelDataObject(FStudyTreeView.Selected.Parent.Data);
            LSubAreaDataObject :=  TSubAreaDataObject(FStudyTreeView.Selected.Data);
          end;
        nlScenario:
          begin
            LStudyDataObject := TStudyDataObject(FStudyTreeView.Selected.Parent.Parent.Parent.Data);
            LModelDataObject :=  TModelDataObject(FStudyTreeView.Selected.Parent.Parent.Data);
            LSubAreaDataObject :=  TSubAreaDataObject(FStudyTreeView.Selected.Parent.Data);
            LScenarioDataObject := TScenarioDataObject(FStudyTreeView.Selected.Data);
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

function TStudyAreaSelectionDialog.CurrentNodeIsScenarioWithNoData: boolean;
const OPNAME = 'TStudyAreaSelectionDialog.CurrentNodeIsScenarioWithNoData';
var
  LStudyFields: TStudyFields;
  LNodeLevel: TNodeLevel;
begin
  Result := False;
  try
    if Assigned(FStudyTreeView.Selected) and
       Assigned(FStudyTreeView.Selected.Data) then
    begin
      LStudyFields := TStudyFields.Create;
      try
        LNodeLevel   := TStudySelectionNodeData(FStudyTreeView.Selected.Data).NodeLevel;
        if (LNodeLevel = nlScenario) then
        begin
          if PopulateFieldsFromSelectionDialog(LNodeLevel,LStudyFields) then
          begin
            Result := not LStudyFields.FFilesLoaded;
          end;
        end;
      finally
        FreeAndNil(LStudyFields);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyAreaSelectionDialog.CurrentNodeIsSelectedStudy: boolean;
const OPNAME = 'TStudyAreaSelectionDialog.CurrentNodeIsSelectedStudy';
var
  LStudyFields: TStudyFields;
  LNodeLevel: TNodeLevel;
begin
  Result := False;
  try
    if Assigned(FStudyTreeView.Selected) and
       Assigned(FStudyTreeView.Selected.Data) then
    begin
      LStudyFields := TStudyFields.Create;
      try
        LNodeLevel   := TStudySelectionNodeData(FStudyTreeView.Selected.Data).NodeLevel;
        if PopulateFieldsFromSelectionDialog(LNodeLevel,LStudyFields) then
        begin
          case LNodeLevel of
            nlStudy   : Result := (LStudyFields.FStudyAreaName = FAppModules.StudyArea.StudyAreaCode);
            nlModel   : Result := (LStudyFields.FStudyAreaName = FAppModules.StudyArea.StudyAreaCode) and
                                  (LStudyFields.FModel = FAppModules.StudyArea.ModelCode);
            nlSubArea : Result := (LStudyFields.FStudyAreaName = FAppModules.StudyArea.StudyAreaCode) and
                                  (LStudyFields.FModel = FAppModules.StudyArea.ModelCode) and
                                  (LStudyFields.FSubArea = FAppModules.StudyArea.SubAreaCode);
            nlScenario: Result := (LStudyFields.FStudyAreaName = FAppModules.StudyArea.StudyAreaCode) and
                                  (LStudyFields.FModel = FAppModules.StudyArea.ModelCode) and
                                  (LStudyFields.FSubArea = FAppModules.StudyArea.SubAreaCode) and
                                  (LStudyFields.FScenario = FAppModules.StudyArea.ScenarioCode);
            else
              Result := True;
          end;
        end;
      finally
        FreeAndNil(LStudyFields);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.mnuExitClick(Sender: TObject);
const OPNAME = 'TStudyAreaSelectionDialog.mnuExitClick';
begin
  try
    Self.ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyAreaSelectionDialog.CurrentNodeIsRainfallProjectGauges: boolean;
const OPNAME = 'TStudyAreaSelectionDialog.CurrentNodeIsRainfallProjectGauges';
var
  LStudyFields: TStudyFields;
  LNodeLevel: TNodeLevel;
begin
  Result := False;
  try
    if Assigned(FStudyTreeView.Selected) and
      Assigned(FStudyTreeView.Selected.Data) then
    begin
      LStudyFields := TStudyFields.Create;
      try
        LNodeLevel   := TStudySelectionNodeData(FStudyTreeView.Selected.Data).NodeLevel;
        if (LNodeLevel = nlScenario) then
        begin
          if PopulateFieldsFromSelectionDialog(LNodeLevel,LStudyFields) then
          begin
            Result := LStudyFields.FScenario = UAbstractObject.CProjectGauges;
          end;
        end;
      finally
        FreeAndNil(LStudyFields);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TStudyAreaSelectionDialog.GetSelectedNode(AStudy, AModel,ASubArea, AScenario: string):TTreeNode;
const OPNAME = 'TStudyAreaSelectionDialog.GetSelectedNode';
var
  LIndex: integer;
  LCurrentNode: TTreeNode;

  LLevel: TNodeLevel;
  LStudy: TStudyDataObject;
  LModel: TModelDataObject;
  LSubArea:TSubAreaDataObject;
  LScenario: TScenarioDataObject;
begin
  Result := nil;
  try
    if(Trim(AStudy) = '') then Exit;
    if(Trim(AScenario) <> '') then
      LLevel := nlScenario
    else if(Trim(ASubArea) <> '') then
      LLevel := nlSubArea
    else if(Trim(AModel) <> '') then
      LLevel := nlModel
    else if(Trim(AStudy) <> '') then
      LLevel := nlStudy
    else
      Exit;

    for LIndex := 0 to  FStudyTreeView.Items.Count -1 do
    begin
      LCurrentNode := FStudyTreeView.Items[LIndex];
      if (LCurrentNode.Data = nil) then Continue;
      case LLevel of
        nlStudy :
          begin
            if(TStudySelectionNodeData(LCurrentNode.Data).NodeLevel = nlStudy) then
            begin
              LStudy := TStudyDataObject(LCurrentNode.Data);
              if(LStudy.Study = AStudy) then
                Result := LCurrentNode;
            end;
          end;
        nlModel    :
          begin
            if(TStudySelectionNodeData(LCurrentNode.Data).NodeLevel = nlModel) then
            begin
              LModel := TModelDataObject(LCurrentNode.Data);
              if(LModel.Study.Study = AStudy)  and
                (LModel.SubModel = AModel)then
                Result := LCurrentNode;
            end;
          end;
        nlSubArea  :
          begin
            if(TStudySelectionNodeData(LCurrentNode.Data).NodeLevel = nlSubArea) then
            begin
              LSubArea := TSubAreaDataObject(LCurrentNode.Data);
              if(LSubArea.Model.Study.Study = AStudy) and
                 (LSubArea.Model.SubModel =  AModel) and
                 (LSubArea.SubArea = ASubArea)then
                Result := LCurrentNode;
            end;
          end;
        nlScenario :
          begin
            if(TStudySelectionNodeData(LCurrentNode.Data).NodeLevel = nlScenario) then
            begin
              LScenario := TScenarioDataObject(LCurrentNode.Data);
              if(LScenario.SubArea.Model.Study.Study = AStudy) and
                 (LScenario.SubArea.Model.SubModel =  AModel) and
                 (LScenario.SubArea.SubArea = ASubArea) and
                 (LScenario.Scenario = AScenario)then
                Result := LCurrentNode;
            end;
          end;
      end;
      if(Result <> nil) then  Break;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.SelectedNode(AStudy, AModel, ASubArea,AScenario: string);
const OPNAME = 'TStudyAreaSelectionDialog.SelectedNode';
begin
  try
    FPreSelectedNode := GetSelectedNode(AStudy, AModel, ASubArea,AScenario);
    if Assigned(FPreSelectedNode) then
      FStudyTreeView.Selected := FPreSelectedNode;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudyAreaSelectionDialog.SetExpandedNodes(AList : TStrings);
const OPNAME = 'TStudyAreaSelectionDialog.SetExpandedNodes';
var
  LCurrentNode : TTreeNode;
  LIndex : integer;
begin
  try
    if (AList <> nil) then
    begin
      for LIndex := 0 to  FStudyTreeView.Items.Count -1 do
      begin
        LCurrentNode := FStudyTreeView.Items[LIndex];
        if LCurrentNode <> nil then
        begin
           case LCurrentNode.Level of
           0 :
             begin
               if AList.IndexOf(LCurrentNode.Text) >= 0 then
                 LCurrentNode.Expanded := True;
             end;
           1 :
             begin
               if AList.IndexOf(LCurrentNode.Parent.Text+LCurrentNode.Text) >= 0 then
               begin
                 LCurrentNode.Parent.Expanded := True;
                 LCurrentNode.Expanded := True;
               end;
             end;
           2 :
             begin
               if AList.IndexOf(LCurrentNode.Parent.Parent.Text+LCurrentNode.Parent.Text+LCurrentNode.Text) >= 0 then
               begin
                 LCurrentNode.Parent.Parent.Expanded := True;
                 LCurrentNode.Parent.Expanded := True;
                 LCurrentNode.Expanded := True;
               end;
             end;
           end;
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TStudyAreaSelectionDialog.DoCancelButtonClick(Sender: TObject);
const OPNAME = 'TStudyAreaSelectionDialog.DoCancelButtonClick';
begin
  try
    Close;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

{function TStudyAreaSelectionDialog.OnHelpRequest(Command: Word; Data: THelpEventData;var CallHelp: Boolean): Boolean;
const OPNAME = 'TStudyAreaSelectionDialog.OnHelpRequest';
begin
  Result := FALSE;
  try
    if(Data = HC_WaterResourcesYieldModel) then
      Application.OnHelp(Command,Data,CallHelp);
    //Application.HelpContext(Data);

    CallHelp := True;
    Result   := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
 }

function TStudyAreaSelectionDialog.GetGISStudyAreaSelectorPanel: TGISStudyAreaSelectorPanel;
const OPNAME = 'TStudyAreaSelectionDialog.GetGISStudyAreaSelectorPanel';
begin
  Result := nil;
  try
    Result := FGISStudyAreaSelectorPanel;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStudyAreaSelectionDialog.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
const OPNAME = 'TStudyAreaSelectionDialog.FormCanResize';
var
  LDefaltWidth : Integer;
begin
  try
    if Assigned(ButtonPanel) then
    begin
      LDefaltWidth := ButtonPanel.OKButton.ClientWidth +  ButtonPanel.CancelButton.ClientWidth +
                      ButtonPanel.NewButton.ClientWidth + ButtonPanel.CopyButton.ClientWidth +
                      ButtonPanel.EditButton.ClientWidth + ButtonPanel.DeleteButton.ClientWidth+
                      ButtonPanel.ReportButton.ClientWidth + 40;
      if (NewWidth < LDefaltWidth ) then   //565
        Resize := false;

      if (NewHeight <  400) then
        Resize := false;
    end;
   except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TStudyAreaSelectionDialog.GetScenarioDataObjectFromNode(ANode: TTreeNode): TScenarioDataObject;
const OPNAME = 'TStudyAreaSelectionDialog.GetScenarioDataObjectFromNode';
var
  LNode: TTreeNode;
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

end.
