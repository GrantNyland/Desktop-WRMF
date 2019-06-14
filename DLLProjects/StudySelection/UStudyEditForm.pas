unit UStudyEditForm;

interface
{$WARN UNIT_PLATFORM OFF}
uses

  // Delphi VCL, RTL, etc

  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.FileCtrl,
  Contnrs,
  // QDialogs,

  // DWAF/arivia.kom
  UAbstractComponent,
  UCreateWrymForm,
  UHelpContexts,
  UStudyObjects,
  UAbstractObject,
  UDataEditComponent,
  UDataComponent;

type

  TfrmStudyEditForm = class(TAbstractForm)
    pnlStudy                      : TPanel;
    pnlScenario                   : TPanel;
    pnlSubArea                    : TPanel;
    pnlModel                      : TPanel;
    lblModelCaption               : TLabel;
    lblStudyCaption               : TLabel;
    edtStudy                      : TEdit;
    lblStudyLabelCaption          : TLabel;
    edtStudyLabel                 : TEdit;
    lblStudyClientCaption         : TLabel;
    edtStudyClient                : TEdit;
    lblStudyConsultantCaption     : TLabel;
    edtStudyConsultant            : TEdit;
    lblStudyNumberCaption         : TLabel;
    edtStudyNumber                : TEdit;
    dtpStudyDate                  : TDateTimePicker;
    lblStudyDateCaption           : TLabel;
    lblStudyDescrCaption          : TLabel;
    memoStudyDescr                : TMemo;
    lblSubAreaCaption             : TLabel;
    edtSubArea                    : TEdit;
    lblSubAreaLabelCaption        : TLabel;
    edtSubAreaLabel               : TEdit;
    lblSubAreDescr                : TLabel;
    memoSubAreaDescr              : TMemo;
    lblScenarioCaption            : TLabel;
    edtScenario                   : TEdit;
    lblScenarioLabelCaption       : TLabel;
    edtScenarioLabel              : TEdit;
    memoScenarioDescr             : TMemo;
    lblScenarioDescr              : TLabel;
    edtScenarioFilesPath          : TEdit;
    lblScenarioFilesPathCaption   : TLabel;
    cmbModel                      : TComboBox;
    btnScenarioPath               : TButton;
    dlgWYRMFileSelector           : TOpenDialog;
    memoWrymContents              : TMemo;
    lblWrymFileContents           : TLabel;
    chkbFilesLoaded               : TCheckBox;
    cmbCalenderStartMonth         : TComboBox;
    lblCalenderMonth              : TLabel;
    pcModel                       : TPageControl;
    tbsAll                        : TTabSheet;
    tbsYieldModel                 : TTabSheet;
    pnlYieldModel                 : TPanel;
    tbsHydrology: TTabSheet;
    tbsRainfall                   : TTabSheet;
    pnlButtons                    : TPanel;
    btnReset                      : TBitBtn;
    btnCancel                     : TBitBtn;
    btnSave                       : TBitBtn;
    ProgressBar1                  : TProgressBar;
    lblVersion                    : TLabel;
    edtRainfallDefaultDirectory   : TEdit;
    lblRainfalldir                : TLabel;
    btnSetDirectory               : TButton;
    cmbVersion                    : TComboBox;
    btnScenarioPathCreate         : TButton;
    edtStudyShapeFileName         : TEdit;
    Label2                        : TLabel;
    edtSubAreaShapeFileName       : TEdit;
    Label4                        : TLabel;
    btnLoadSubAreaShapeFiles      : TButton;
    btnLoadStudyShapeFiles        : TButton;
    lblVersionDescr: TLabel;
    tbsStomsa: TTabSheet;
    lblStomsaHydrologypath: TLabel;
    edtStomsaHydrologypath: TEdit;
    btnStomsaHydrologypath: TButton;
    chkbStudyDataLocked: TCheckBox;
    lblHydrologyInputDir: TLabel;
    lblHydrologyIOutputDir: TLabel;
    lblHydrologyIDebugRequired: TLabel;
    lblHydrologyIDebugStart: TLabel;
    lblHydrologyIDebugEnd: TLabel;
    lblHydrologyISumRequired: TLabel;
    lblHydrologyISimStart: TLabel;
    lblHydrologyISimEnd: TLabel;
    btnHydrologyIInputDir: TSpeedButton;
    btnHydrologyIOuputDir: TSpeedButton;
    edtHydrologyInputDir: TEdit;
    edtHydrologyIOutputDir: TEdit;
    chkHydrologyIDebugRequired: TCheckBox;
    edtHydrologyIDebugStart: TEdit;
    edtHydrologyIDebugEnd: TEdit;
    chkHydrologyISumRequired: TCheckBox;
    edtHydrologyISimStart: TEdit;
    edtHydrologyISimEnd: TEdit;
    lblNetworkID: TLabel;
    edtNetworkID: TEdit;
    lblNetworkCode: TLabel;
    edtNetworkCode: TEdit;
    lblMaxChars: TLabel;
    tbsDDTS: TTabSheet;
    memoDDTSContents: TMemo;
    Button1: TButton;
    Button2: TButton;
    edtDDTSFilesPath: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    procedure edtStudyExit(Sender: TObject);
    procedure edtSubAreaExit(Sender: TObject);
    procedure edtScenarioExit(Sender: TObject);
    procedure cmbModelChange(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cmbModelExit(Sender: TObject);
    procedure btnScenarioPathClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkbFilesLoadedClick(Sender: TObject);
    procedure edtScenarioFilesPathExit(Sender: TObject);
    procedure cmbCalenderStartMonthChange(Sender: TObject);
    procedure btnSetDirectoryClick(Sender: TObject);
    procedure cmbVersionChange(Sender: TObject);
    procedure btnScenarioPathCreateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure btnLoadStudyShapeFilesClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,NewHeight: Integer; var Resize: Boolean);
    procedure btnStomsaHydrologypathClick(Sender: TObject);
    procedure chkbStudyDataLockedClick(Sender: TObject);
    procedure edtScenarioChange(Sender: TObject);
    procedure btnHydrologyIInputDirClick(Sender: TObject);
    procedure btnHydrologyIOuputDirClick(Sender: TObject);
  protected
    FNodeLevel: TNodeLevel;
    FFormActionState : TFormActionState;
    FModelHasChanged,
    FStudyHasChanged,
    FSubAreHasChanged,
    FScenarioHasChanged: boolean;
    FStudyFields: TStudyFields;
    FWrymForm:TfrmCreateWrymForm;

    procedure DisableAllControls;
    procedure EnableAllControls;

    procedure SetModelState(AEnabled: boolean);
    procedure SetStudyState(AEnabled: boolean);
    procedure SetSubAreaState(AEnabled: boolean);
    procedure SetScenarioState(AEnabled: boolean);
    procedure DoOnKeypress ( Sender: TObject; var Key : Char );

    procedure DisableStudyIndexState;
    procedure DisableSubAreaIndexState;
    procedure DisableScenarioIndexState;

    procedure SetFormInitialState;
    procedure ClearModel;
    procedure ClearStudy;
    procedure ClearSubAreaData;
    procedure ClearScenarioData;
    procedure UpdateStudyVersionDescriptionLabel;
    function ValidateIndexFields: boolean;
    function CheckPathDosCompatibilty: boolean;
    function RelatedFilesFound ( aSelectedFile : string ): boolean;
    function GetModel : string;
    procedure AssignHelpContext; override;
    procedure SetTabsheets;
    procedure RunTabsheetsManager;
    procedure CopyClassRAndPatRUtiliesToNewFolder ( aOldDirFile, aNewDirFile : string );
    procedure SelectModelName(AStudyFields: TStudyFields);
    procedure PopulateStudy(AStudyFields: TStudyFields);
    procedure PopulateSubArea(AStudyFields: TStudyFields);
    procedure PopulateModelSubarea;
    procedure PopulateScenario(AStudyFields: TStudyFields);
    //procedure PopulateHydrologyScenario(AStudyFields: TStudyFields);
    procedure PopulateModelVersion;
    procedure SelectStudyDataForModel(AStudyFields: TStudyFields);
    //function OnHelpRequest(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
    procedure SetModelLabels(AModelName : string);
  public
    function LanguageHasChanged: boolean; override;
    procedure ShowProgres(AProgress: String; ACurrentPos,AMaxPos: integer);
    function PopulateEditDialog(AStudyFields: TStudyFields): boolean;
    function ValidateEditDialog(AStudyFields: TStudyFields): boolean;
    function Get_Set_NetworkPropertiesCommText: string;
    property FormActionState : TFormActionState read FFormActionState write FFormActionState;
    property StudyNameHasChanged : boolean read FStudyHasChanged write FStudyHasChanged;
    property SubAreHasChanged : boolean read FSubAreHasChanged write FSubAreHasChanged;
    property ScenarioHasChanged : boolean read FScenarioHasChanged write FScenarioHasChanged;
    property NodeLevel : TNodeLevel read FNodeLevel write FNodeLevel;
  end;

implementation

uses
  DB,
  UUtilities,
  UDataSetType,
  //UHydroDBManager,
  UErrorHandlingOperations;

{$R *.dfm}

procedure TfrmStudyEditForm.FormCreate(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.FormCreate';
begin
  try;
    inherited;
    FWrymForm := nil;
    //OnHelp := OnHelpRequest;
   except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfrmStudyEditForm.AssignHelpContext;
const OPNAME = 'TfrmStudyEditForm.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                 HC_WaterResourcesYieldModel);
    SetControlHelpContext(cmbVersion,           HC_RunDescription);
    SetControlHelpContext(edtStudyClient,       HC_RunDescription);
    SetControlHelpContext(edtStudyConsultant,   HC_RunDescription);
    SetControlHelpContext(cmbModel,             HC_RunDescription);
    SetControlHelpContext(edtScenario,          HC_RunDescription);
    SetControlHelpContext(memoScenarioDescr,    HC_RunDescription);
    SetControlHelpContext(edtScenarioLabel,     HC_RunDescription);
    SetControlHelpContext(edtStudy,             HC_RunDescription);
    SetControlHelpContext(dtpStudyDate,         HC_RunDescription);
    SetControlHelpContext(memoStudyDescr,       HC_RunDescription);
    SetControlHelpContext(edtStudyLabel,        HC_RunDescription);
    SetControlHelpContext(edtStudyNumber,       HC_RunDescription);
    SetControlHelpContext(edtSubArea,           HC_RunDescription);
    SetControlHelpContext(memoSubAreaDescr,     HC_RunDescription);
    SetControlHelpContext(edtSubAreaLabel,      HC_RunDescription);
    SetControlHelpContext(edtScenarioFilesPath, HC_DataFileLocation);
    SetControlHelpContext(memoWrymContents,     HC_DataFileLocation);
    SetControlHelpContext(cmbCalenderStartMonth,HC_AnalysisPeriod);
    SetControlHelpContext(btnReset,             HC_WaterResourcesYieldModel);
    SetControlHelpContext(btnCancel,            HC_WaterResourcesYieldModel);
    SetControlHelpContext(btnSave,              HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TfrmStudyEditForm.LanguageHasChanged: boolean;
const OPNAME = 'TfrmStudyEditForm.LanguageHasChanged';
begin
  Result := False;
  try
    lblModelCaption.Caption := FAppModules.Language.GetString(ClassName + '.ModelLabel') + ' :';
    lblStudyCaption.Caption := FAppModules.Language.GetString(ClassName + '.StudyLabel') + ' :';
    {cmbModel.Items.Clear;
    cmbModel.Items.Add(FAppModules.Language.GetString('Model.Hydrology'));
    cmbModel.Items.Add(FAppModules.Language.GetString('Model.Rainfall'));
    cmbModel.Items.Add(FAppModules.Language.GetString('Model.Yield'));
    cmbModel.Items.Add(FAppModules.Language.GetString('Model.Planning'));
    cmbModel.Items.Add(FAppModules.Language.GetString('Model.BSPPS'));
    cmbModel.Items.Add(FAppModules.Language.GetString('Model.Stomsa'));
    cmbModel.Items.Add(FAppModules.Language.GetString('Model.IFR'));
    cmbModel.Items.Add(FAppModules.Language.GetString('Model.DailyDiversion'));}
    SetTabsheets;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.SetTabsheets;
const OPNAME = 'TfrmStudyEditForm.SetTabsheets';
var
  LModelCode : string;
begin
  try
    with (pcModel) do
    begin
      ActivePage := tbsAll;
      tbsAll.Caption           := FAppModules.Language.GetString('TabCaption.All');
      tbsAll.TabVisible        := true;

      LModelCode := '';
      if(FStudyFields <> nil) then
        LModelCode := FStudyFields.FSubModel;

      SetModelLabels(LModelCode);

      tbsYieldModel.TabVisible := false;
      tbsHydrology.Caption     := FAppModules.Language.GetString('TabCaption.Hydrology');
      tbsHydrology.TabVisible  := false;
      tbsRainfall.Caption      := FAppModules.Language.GetString('TabCaption.RainfallModel');
      tbsRainfall.TabVisible   := false;
      tbsStomsa.Caption         := FAppModules.Language.GetString('TabCaption.Stomsa');
      tbsStomsa.TabVisible      := false;

      tbsDDTS.Caption         := 'Dam Daily Time Step';
      tbsDDTS.TabVisible      := false;
    end;
  except on E: Exception do HandleError( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.DisableStudyIndexState;
const OPNAME = 'TfrmStudyEditForm.DisableStudyIndexState';
begin
  try
    lblStudyCaption.Enabled             := False;
    edtStudy.Enabled                    := False;
    edtStudy.Color                      := clBtnFace;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.DisableSubAreaIndexState;
const OPNAME = 'TfrmStudyEditForm.DisableSubAreaIndexState';
begin
  try
    lblSubAreaCaption.Enabled           := False;
    edtSubArea.Enabled                  := False;
    edtSubArea.Color                    := clBtnFace;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.DisableScenarioIndexState;
const OPNAME = 'TfrmStudyEditForm.DisableScenarioIndexState';
begin
  try
    lblScenarioCaption.Enabled          := False;
    edtScenario.Enabled                 := False;
    edtScenario.Color                   := clBtnFace;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.SetModelState(AEnabled: boolean);
const OPNAME = 'TfrmStudyEditForm.SetModelState';
var
  LColor: TColor;
begin
  try
    AEnabled := AEnabled and (cmbModel.Items.Count > 1);
    if AEnabled then
       LColor := clWindow
    else
       LColor := clBtnFace;

    lblModelCaption.Enabled := AEnabled;
    if AEnabled then
    begin
      cmbModel.Enabled        := AEnabled;
      cmbModel.Color          := LColor;
    end
    else
    begin
      cmbModel.Enabled        := AEnabled;
      cmbModel.Color          := LColor;
      cmbModel.OnKeyPress     := DoOnKeypress;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.SetStudyState(AEnabled: boolean);
const OPNAME = 'TfrmStudyEditForm.SetStudyState';
var
  LColor: TColor;
begin
  try
    if AEnabled then
       LColor := clWindow
    else
       LColor := clBtnFace;

    lblStudyCaption.Enabled             := AEnabled;
    edtStudy.Enabled                    := AEnabled;
    edtStudy.Color                      := LColor;
    edtStudy.OnKeyPress                 := DoOnKeypress;

    lblStudyLabelCaption.Enabled        := AEnabled;
    edtStudyLabel.Enabled               := AEnabled;
    edtStudyLabel.Color                 := LColor;
    edtStudyLabel.OnKeyPress            := DoOnKeypress;

    lblStudyClientCaption.Enabled       := AEnabled;
    edtStudyClient.Enabled              := AEnabled;
    edtStudyClient.Color                := LColor;
    edtStudyClient.OnKeyPress           := DoOnKeypress;

    lblStudyConsultantCaption.Enabled   := AEnabled;
    edtStudyConsultant.Enabled          := AEnabled;
    edtStudyConsultant.Color            := LColor;
    edtStudyConsultant.OnKeyPress       := DoOnKeypress;

    lblStudyNumberCaption.Enabled       := AEnabled;
    edtStudyNumber.Enabled              := AEnabled;
    edtStudyNumber.Color                := LColor;
    edtStudyNumber.OnKeyPress           := DoOnKeypress;

    lblStudyDateCaption.Enabled         := AEnabled;
    dtpStudyDate.Enabled                := AEnabled;
    dtpStudyDate.Color                  := LColor;
    dtpStudyDate.OnKeyPress             := DoOnKeypress;

    lblStudyDescrCaption.Enabled        := AEnabled;
    memoStudyDescr.Enabled              := AEnabled;
    memoStudyDescr.Color                := LColor;
    memoStudyDescr.OnKeyPress           := DoOnKeypress;

    edtStudyShapeFileName.Enabled       := AEnabled;
    edtStudyShapeFileName.Color         := LColor;
    edtStudyShapeFileName.OnKeyPress    := DoOnKeypress;
    
    btnLoadStudyShapeFiles.Enabled      := AEnabled;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.SetSubAreaState(AEnabled: boolean);
const OPNAME = 'TfrmStudyEditForm.SetSubAreaState';
var
  LColor: TColor;
begin
  try
    if AEnabled then
       LColor := clWindow
    else
       LColor := clBtnFace;

    lblSubAreaCaption.Enabled           := AEnabled;
    edtSubArea.Enabled                  := AEnabled;
    edtSubArea.Color                    := LColor;
    edtSubArea.OnKeyPress               := DoOnKeypress;

    lblSubAreaLabelCaption.Enabled      := AEnabled;
    edtSubAreaLabel.Enabled             := AEnabled;
    edtSubAreaLabel.Color               := LColor;
    edtSubAreaLabel.OnKeyPress          := DoOnKeypress;

    lblSubAreDescr.Enabled              := AEnabled;
    memoSubAreaDescr.Enabled            := AEnabled;
    memoSubAreaDescr.Color              := LColor;
    memoSubAreaDescr.OnKeyPress         := DoOnKeypress;

   edtSubAreaShapeFileName.Enabled     := AEnabled;
   edtSubAreaShapeFileName.Color       := LColor;
   edtSubAreaShapeFileName.OnKeyPress  := DoOnKeypress;
   btnLoadSubAreaShapeFiles.Enabled    := AEnabled;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.SetScenarioState(AEnabled: boolean);
const OPNAME = 'TfrmStudyEditForm.SetScenarioState';
var
  LColor: TColor;
begin
  try
    if AEnabled then
       LColor := clWindow
    else
       LColor := clBtnFace;

    lblScenarioCaption.Enabled          := AEnabled;
    edtScenario.Enabled                 := AEnabled;
    edtScenario.Color                   := LColor;
    edtScenario.OnKeyPress              := DoOnKeypress;

    lblScenarioLabelCaption.Enabled     := AEnabled;
    edtScenarioLabel.Enabled            := AEnabled;
    edtScenarioLabel.Color              := LColor;
    edtScenarioLabel.OnKeyPress         := DoOnKeypress;

    //lblScenarioFilesPrefixCaption.Enabled := AEnabled;
    //edtScenarioFilesPrefix.Enabled        := AEnabled;
    //edtScenarioFilesPrefix.Color          := LColor;

    lblScenarioFilesPathCaption.Enabled     := AEnabled;
    edtScenarioFilesPath.Enabled            := AEnabled;

    edtRainfallDefaultDirectory.Enabled     := False;//AEnabled;
    edtRainfallDefaultDirectory.Color       := clBtnFace;  //LColor;
    edtRainfallDefaultDirectory.OnKeyPress  := DoOnKeypress;
    lblRainfalldir.Enabled              := AEnabled;
    btnSetDirectory.Enabled             := AEnabled;
    edtScenarioFilesPath.Color          := LColor;
    edtScenarioFilesPath.OnKeyPress     := DoOnKeypress;

    btnScenarioPath.Enabled             := AEnabled;
    btnScenarioPathCreate.Enabled       := AEnabled;

    lblScenarioDescr.Enabled            := AEnabled;
    memoScenarioDescr.Enabled           := AEnabled;
    memoScenarioDescr.Color             := LColor;
    memoScenarioDescr.OnKeyPress        := DoOnKeypress;

    lblCalenderMonth.Enabled            := AEnabled;
    cmbCalenderStartMonth.Enabled       := AEnabled;
    cmbCalenderStartMonth.Color         := LColor;
    cmbCalenderStartMonth.OnKeyPress    := DoOnKeypress;

    lblVersion.Enabled                  := AEnabled;
    cmbVersion.Enabled                  := AEnabled;
    cmbVersion.Color                    := LColor;
    cmbVersion.OnKeyPress               := DoOnKeypress;

    chkbFilesLoaded.Enabled             := AEnabled;
    chkbStudyDataLocked.Enabled         := AEnabled;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.ClearStudy;
const OPNAME = 'TfrmStudyEditForm.ClearStudy';
begin
  try
    edtStudy.Text := '';
    edtStudyLabel.Text := '';
    edtStudyClient.Text := '';
    edtStudyConsultant.Text := '';
    edtStudyNumber.Text := '';
    edtStudyShapeFileName.Text := '';
    dtpStudyDate.Date := Date;
    memoStudyDescr.Text := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.ClearModel;
const OPNAME = 'TfrmStudyEditForm.ClearModel';
begin
  try
    cmbModel.ItemIndex := -1;
    cmbModel.Text      := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.ClearSubAreaData;
const OPNAME = 'TfrmStudyEditForm.ClearSubAreaData';
begin
  try
    edtSubArea.Text := '';
    edtSubAreaLabel.Text := '';
    edtSubAreaShapeFileName.Text := '';
    memoSubAreaDescr.Lines.Text := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.ClearScenarioData;
const OPNAME = 'TfrmStudyEditForm.ClearScenarioData';
begin
  try
    edtScenario.Text := '';
    edtScenarioLabel.Text := '';
    //edtScenarioFilesPrefix.Text := '';
    edtScenarioFilesPath.Text := '';
    edtDDTSFilesPath.Text     := '';
    edtRainfallDefaultDirectory.Text := '';
    memoScenarioDescr.Lines.Text := '';
    chkbFilesLoaded.Checked := False;
    chkbStudyDataLocked.Checked := False;
    cmbCalenderStartMonth.ItemIndex := 09;
    cmbVersion.ItemIndex := -1;

    edtNetworkID.Text := '';
    edtNetworkCode.Text := '';
    edtHydrologyInputDir.Text := '';
    edtHydrologyIOutputDir.Text := '';
    edtHydrologyIDebugStart.Text := '';
    edtHydrologyIDebugEnd.Text := '';
    edtHydrologyISimStart.Text := '';
    edtHydrologyISimEnd.Text := '';
    chkHydrologyIDebugRequired.Checked := False;
    chkHydrologyISumRequired.Checked := False;

    UpdateStudyVersionDescriptionLabel;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.DisableAllControls;
const OPNAME = 'TfrmStudyEditForm.DisableAllControls';
begin
  try
    btnSave.Enabled := False;
    SetModelState(False);
    SetStudyState(False);
    SetSubAreaState(False);
    SetScenarioState(False);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.EnableAllControls;
const OPNAME = 'TfrmStudyEditForm.EnableAllControls';
begin
  try
    SetModelState(True);
    SetStudyState(True);
    SetSubAreaState(True);
    SetScenarioState(True);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.ShowProgres(AProgress: String; ACurrentPos,AMaxPos: integer);
const OPNAME = 'TfrmStudyEditForm.ShowProgres';
begin
  try
    if(ACurrentPos <= AMaxPos) then
    begin
      ProgressBar1.Max := AMaxPos;
      ProgressBar1.Position := ACurrentPos;
    end;
    Application.ProcessMessages;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.SelectModelName(AStudyFields: TStudyFields);
const OPNAME = 'TfrmStudyEditForm.SelectModelName';
begin
  try
    cmbModel.ItemIndex := -1;
    if (cmbModel.Items.Count > 0) then
    begin
      if (Trim(AStudyFields.FModel) = '') then
      begin
        cmbModel.ItemIndex := -1;
      end
      else
      begin
        if(cmbModel.Items.IndexOf(Trim(AStudyFields.FModel)) >= 0)then
        cmbModel.ItemIndex := cmbModel.Items.IndexOf(Trim(AStudyFields.FModel))
      end;
      if(cmbModel.ItemIndex > -1) then
        cmbModel.Text := cmbModel.Items[cmbModel.ItemIndex];
      AStudyFields.FModel := cmbModel.Text;
    end;
    PopulateModelVersion;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.PopulateStudy(AStudyFields: TStudyFields);
const OPNAME = 'TfrmStudyEditForm.PopulateStudy';
begin
  try
    if ( FNodeLevel = nlStudy ) and ( FFormActionState =  fasAdd ) then
      Exit;
      
    edtStudy.Text        := AStudyFields.FStudyAreaName;

    if(AStudyFields.FStudyDate <= 0.0) then
      dtpStudyDate.DateTime       :=  Now
    else
      dtpStudyDate.DateTime       :=  AStudyFields.FStudyDate;
    edtStudyConsultant.Text       :=  AStudyFields.FConsultant;
    edtStudyClient.Text           :=  AStudyFields.FClient;
    edtStudyNumber.Text           :=  AStudyFields.FStudyNumber;
    edtStudyLabel.Text            :=  AStudyFields.FStudyLabel;
    memoStudyDescr.Lines.Text     :=  AStudyFields.FStudyAreaDescr;
    edtStudyShapeFileName.Text    :=  AStudyFields.FStudyShapeFileName;

    SetModelLabels(AStudyFields.FSubModel);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.PopulateModelSubarea;
const OPNAME = 'TfrmStudyEditForm.PopulateModelSubarea';
begin
  try
    if(FFormActionState =  fasAdd) then
    begin
      if(cmbModel.Text = CDailyDiversion) or (cmbModel.Text = CIFRPreProcessor) or (cmbModel.Text = CDamSedimentation) or (cmbModel.Text = CStomsa)  then
      begin
        if(edtSubArea.Text = '') then
          edtSubArea.Text := edtStudy.Text;
        if(edtSubAreaLabel.Text = '') then
          edtSubAreaLabel.Text := edtStudyLabel.Text;
        if(edtSubAreaShapeFileName.Text = '') then
          edtSubAreaShapeFileName.Text := edtStudyShapeFileName.Text;
        if(memoSubAreaDescr.Lines.Text = '') then
          memoSubAreaDescr.Lines.Text := memoStudyDescr.Text;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.PopulateSubArea(AStudyFields: TStudyFields);
const OPNAME = 'TfrmStudyEditForm.PopulateSubArea';
begin
  try
   if not ( FFormActionState =  fasAdd ) or not ( edtSubArea.Enabled ) then
    begin
      edtSubArea.Text               := AStudyFields.FSubArea;
      edtSubAreaLabel.Text          := AStudyFields.FSubAreaLabel;
      edtSubAreaShapeFileName.Text  := AStudyFields.FSubAreaShapeFileName;
      memoSubAreaDescr.Lines.Text   := AStudyFields.FSubAreaDescr;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.PopulateScenario(AStudyFields: TStudyFields);
const OPNAME = 'TfrmStudyEditForm.PopulateScenario';
begin
  try
    if not ( FFormActionState =  fasAdd ) or not ( edtScenario.Enabled ) then
    begin
      edtScenario.Text                 := AStudyFields.FScenario;
      edtScenarioLabel.Text            := AStudyFields.FScenarioLabel;
      memoScenarioDescr.Lines.Text     := AStudyFields.FScenarioDescr;
      edtScenarioFilesPath.Text        := AStudyFields.FDataFilesPath;
      if (AStudyFields.FModel = CDDTS) then
        edtDDTSFilesPath.Text            := AStudyFields.FDataFilesPath;
      edtRainfallDefaultDirectory.Text := AStudyFields.FDataFilesPath;
      chkbFilesLoaded.Checked          := AStudyFields.FFilesLoaded;
      chkbStudyDataLocked.Checked      := not AStudyFields.FEditable;
      cmbCalenderStartMonth.ItemIndex  := AStudyFields.FCalenderStartMonth -1;
      if (AStudyFields.FModel = CPlanning) then
        cmbVersion.ItemIndex             := 0
      else
        cmbVersion.ItemIndex             := cmbVersion.Items.IndexOf(AStudyFields.FVersion);
      UpdateStudyVersionDescriptionLabel;

      edtStomsaHydrologypath.Text      := AStudyFields.FDataFilesPath;
      //if(AStudyFields.FSubModel = CHydrology) then
      //  PopulateHydrologyScenario(AStudyFields)
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfrmStudyEditForm.PopulateEditDialog(AStudyFields: TStudyFields): boolean;
const OPNAME = 'TfrmStudyEditForm.PopulateEditDialog';
begin
  Result := False;
  try
    btnSave.Enabled := False;
    if not Assigned(AStudyFields) then
       raise Exception.Create('Study fields parameter is not yet assigned.');
    FStudyFields := AStudyFields;
    ClearStudy;
    ClearSubAreaData;
    ClearScenarioData;
    SetTabsheets;
    SetFormInitialState;
    if (FNodeLevel = nlStudy) and (FFormActionState =  fasAdd) then
      Exit;
    SelectModelName(AStudyFields);
    PopulateStudy(AStudyFields);
    PopulateSubArea(AStudyFields);
    PopulateScenario(AStudyFields);
    RunTabsheetsManager;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.SetFormInitialState;
const OPNAME = 'TfrmStudyEditForm.SetFormInitialState';
begin
  try
    DisableAllControls;
    ProgressBar1.Position := 0;
    FModelHasChanged := False;
    case FormActionState of
      fasEdit:
        begin
         case FNodeLevel of
           nlStudy:
             begin
               SetStudyState(True);
               DisableStudyIndexState;
             end;
           nlModel:
           begin
           end;
           nlSubArea:
             begin
               SetSubAreaState(True);
               DisableSubAreaIndexState
             end;
           nlScenario:
             begin
               SetScenarioState(True);
               DisableScenarioIndexState;
             end;
         end;//case
        end;
      fasDelete:
        begin
         btnSave.Enabled := (Trim(cmbModel.Text) <> '');
         case FNodeLevel of
           nlStudy:
             begin
               SetModelState( false );
               FStudyHasChanged := True;
             end;
           nlModel:
           begin
           end;
           nlSubArea:
             begin
               FSubAreHasChanged := True;
             end;
           nlScenario:
             begin
               FScenarioHasChanged := True;
             end;
         end;//case
        end;
      fasAdd:
        begin
         case FNodeLevel of
           nlStudy:
             begin
               EnableAllControls;
             end;
           nlModel:
             begin
               SetModelState(True);
               SetSubAreaState(True);
               SetScenarioState(True);
               ClearSubAreaData;
               ClearScenarioData;
             end;
           nlSubArea:
             begin
               SetSubAreaState(True);
               SetScenarioState(True);
               ClearSubAreaData;
               ClearScenarioData;
             end;
           nlScenario:
             begin
               SetScenarioState(True);
               ClearScenarioData;
             end;
         end;//case
        end;
    end//Case
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmStudyEditForm.ValidateEditDialog(AStudyFields: TStudyFields): boolean;
const OPNAME = 'TfrmStudyEditForm.ValidateEditDialog';
begin
  Result := False;
  try
    if not Assigned(AStudyFields) then
       raise Exception.Create('Study fields parameter is not yet assigned.');

    AStudyFields.Reset;

    if not ValidateIndexFields then
    Exit;

    AStudyFields.FModel         := cmbModel.Text;
    AStudyFields.FStudyAreaName := edtStudy.Text;
    AStudyFields.FSubArea       := edtSubArea.Text;
    AStudyFields.FScenario      := edtScenario.Text;

    AStudyFields.FStudyDate      := dtpStudyDate.DateTime;
    AStudyFields.FConsultant     := edtStudyConsultant.Text;
    AStudyFields.FClient         := edtStudyClient.Text;
    AStudyFields.FStudyNumber    := edtStudyNumber.Text;
    AStudyFields.FStudyLabel     := edtStudyLabel.Text;
    AStudyFields.FStudyAreaDescr := memoStudyDescr.Lines.Text;


    AStudyFields.FSubAreaLabel := edtSubAreaLabel.Text;
    AStudyFields.FSubAreaDescr := memoSubAreaDescr.Lines.Text;

    AStudyFields.FScenarioLabel   := edtScenarioLabel.Text;
    AStudyFields.FScenarioDescr   := memoScenarioDescr.Lines.Text;
    AStudyFields.FDataFilesPrefix := '';
    AStudyFields.FStudyShapeFileName   := edtStudyShapeFileName.Text;
    AStudyFields.FSubAreaShapeFileName := edtSubAreaShapeFileName.Text;

    AStudyFields.FFilesLoaded        := chkbFilesLoaded.Checked;
    AStudyFields.FEditable           := not chkbStudyDataLocked.Checked;
    AStudyFields.FCalenderStartMonth := cmbCalenderStartMonth.ItemIndex + 1;
    if (AStudyFields.FModel = CPlanning) then
      AStudyFields.FVersion            := '7'
    else
      AStudyFields.FVersion            := cmbVersion.Text;

    if (AStudyFields.FModel = CRainfall) then
    begin
      AStudyFields.FDataFilesPath    := edtRainfallDefaultDirectory.Text;
    end
    else if (AStudyFields.FModel = CStomsa) then
    begin
      AStudyFields.FDataFilesPath    := edtStomsaHydrologypath.Text;
    end
    else if (AStudyFields.FModel = CYield) or (AStudyFields.FModel = CPlanning) then
    begin
      AStudyFields.FDataFilesPath    := edtScenarioFilesPath.Text;
    end
    else if (AStudyFields.FModel = CDDTS) then
    begin
      AStudyFields.FDataFilesPath    := edtDDTSFilesPath.Text;
    end
    else if (AStudyFields.FModel = CHydrology) then
    begin
      AStudyFields.FInputDir             := edtHydrologyInputDir.Text;
      AStudyFields.FOutputDir            := edtHydrologyIOutputDir.Text;
      if chkHydrologyIDebugRequired.Checked then
        AStudyFields.FDebugRequired      := 'Y'
      else
        AStudyFields.FDebugRequired      := 'N';
      AStudyFields.FDebugStartPeriod     := StrToInt(edtHydrologyIDebugStart.Text);
      AStudyFields.FDebudEndPeriod       := StrToInt(edtHydrologyIDebugEnd.Text);
      if chkHydrologyISumRequired.Checked then
        AStudyFields.FSummaryRequired    := 'Y'
      else
        AStudyFields.FSummaryRequired    := 'N';
      AStudyFields.FSimulationStartYear  := StrToInt(edtHydrologyISimStart.Text);
      AStudyFields.FSimulationEndYear    := StrToInt(edtHydrologyISimEnd.Text);
      AStudyFields.FCalenderStartMonth   := StrToIntDef(edtNetworkID.Text,0);
    end;


    Result := (FModelHasChanged or
              FStudyHasChanged or
              FSubAreHasChanged or
              FScenarioHasChanged) and
              (Trim(cmbModel.Text) <> '');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.edtStudyExit(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.edtStudyExit';
begin
       try
    if Sender is TEdit then
    begin
      if TEdit(Sender).Modified then
        FStudyHasChanged := True;
    end
    else if Sender is TMemo then
    begin
      if TMemo(Sender).Modified then
        FStudyHasChanged := True;
    end
    else if Sender is TDateTimePicker then
    begin
      FStudyHasChanged := True;
    end;
    btnSave.Enabled := FStudyHasChanged and (Trim(cmbModel.Text) <> '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.edtSubAreaExit(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.edtSubAreaExit';
begin
  try
    if Sender is TEdit then
    begin
      if TEdit(Sender).Modified then
      FSubAreHasChanged := True;
     end
    else if Sender is TMemo then
    begin
      if TMemo(Sender).Modified then
      FSubAreHasChanged := True;
    end;
    btnSave.Enabled := FSubAreHasChanged and (Trim(cmbModel.Text) <> '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.edtScenarioExit(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.edtScenarioExit';
begin
  try
    if Sender is TEdit then
    begin
      if TEdit(Sender).Modified then
      FScenarioHasChanged := True;
    end
    else if Sender is TMemo then
    begin
      if TMemo(Sender).Modified then
      FScenarioHasChanged := True;
    end
    else if Sender is TComboBox then
    begin
      FScenarioHasChanged := True;
    end;
    btnSave.Enabled := FScenarioHasChanged and (Trim(cmbModel.Text) <> '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.cmbModelChange(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.cmbModelChange';
begin
  try
    if (Trim(cmbModel.Text) = '') then
      btnSave.Enabled := False;
    FStudyFields.FModel := cmbModel.Text;
    SelectStudyDataForModel(FStudyFields);
    PopulateStudy(FStudyFields);
    PopulateModelSubarea;
    RunTabsheetsManager;
    PopulateModelVersion;
    UpdateStudyVersionDescriptionLabel;
    SetModelLabels(cmbModel.Text);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.btnResetClick(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.btnResetClick';
begin
  try
    PopulateEditDialog(FStudyFields)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.cmbModelExit(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.cmbModelExit';
begin
  try
    if(Trim(cmbModel.Text) <> '') and
      (FormActionState = fasDelete) and
      (FNodeLevel =  nlStudy) then
      btnSave.Enabled := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmStudyEditForm.ValidateIndexFields: boolean;
const OPNAME = 'TfrmStudyEditForm.ValidateIndexFields';
begin
  Result := False;
  try
    case FormActionState of
      fasEdit:
        case FNodeLevel of
         nlStudy:
           begin
             Result := (Trim(cmbModel.Text) <> '') and
                       (Trim(edtStudy.Text) <> '') and
                       (Trim(edtStudyLabel.Text) <> '');
           end;
         nlSubArea:
           begin
             Result := (Trim(cmbModel.Text) <> '') and
                       (Trim(edtStudy.Text) <> '') and
                       (Trim(edtStudyLabel.Text) <> '') and
                       (Trim(edtSubArea.Text) <> '') and
                       (Trim(edtSubAreaLabel.Text) <> '');
           end;
         nlScenario:
           begin
             Result := (Trim(cmbModel.Text) <> '') and
                       (Trim(edtStudy.Text) <> '') and
                       (Trim(edtStudyLabel.Text) <> '') and
                       (Trim(edtSubArea.Text) <> '') and
                       (Trim(edtSubAreaLabel.Text) <> '') and
                       (Trim(edtScenario.Text) <> '') and
                       (Trim(edtScenarioLabel.Text) <> '') and
                       (Trim(cmbVersion.Text) <> '');
              if(Trim(cmbModel.Text) = 'Rainfall') then
                Result := ( edtRainfallDefaultDirectory.Text <> '' );
              if(Trim(cmbModel.Text) = 'Yield') or (Trim(cmbModel.Text) = 'Planning')then
                Result := Result and (Trim(edtScenarioFilesPath.Text) <> '') and CheckPathDosCompatibilty ;

           end;
        end;//case FNodeLevel
      fasDelete: Result := True;
      fasAdd:
        begin
             Result := (Trim(cmbModel.Text) <> '') and
                       (Trim(edtStudy.Text) <> '') and
                       (Trim(edtStudyLabel.Text) <> '') and
                       (Trim(edtSubArea.Text) <> '') and
                       (Trim(edtSubAreaLabel.Text) <> '') and
                       (Trim(edtScenario.Text) <> '') and
                       (Trim(edtScenarioLabel.Text) <> '') and
                       (Trim(cmbVersion.Text) <> '');
              if(Trim(cmbModel.Text) = 'Rainfall') then
              begin
                Result := (Trim(cmbModel.Text) <> '') and
                          (Trim(edtStudy.Text) <> '') and
                          (Trim(edtStudyLabel.Text) <> '') and
                          (Trim(edtSubArea.Text) <> '');{ and
                          (Trim(edtSubAreaLabel.Text) <> '') or
                          (Trim(edtScenario.Text) <> '') or
                          (Trim(edtScenarioLabel.Text) <> '');}
                Result := Result or ((edtRainfallDefaultDirectory.Modified) and (edtRainfallDefaultDirectory.Text <> ''));
              end;
              if(Trim(cmbModel.Text) = 'Yield') or (Trim(cmbModel.Text) = 'Planning')then
                Result := Result and (Trim(edtScenarioFilesPath.Text) <> '') and CheckPathDosCompatibilty ;
        end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.btnScenarioPathClick(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.btnScenarioPathClick';
var
  LPath,
  LSelectedDir: string;
begin
  try
    LSelectedDir := '';
    LPath := '';
    if (Trim(edtScenarioFilesPath.Text) <> '') then
      LPath := ExtractFilePath(Trim(edtScenarioFilesPath.Text));
     if (Trim(edtDDTSFilesPath.Text) <> '') then
      LPath := ExtractFilePath(Trim(edtDDTSFilesPath.Text));

    dlgWYRMFileSelector.InitialDir := LPath;
    if dlgWYRMFileSelector.Execute then
    begin
      edtScenarioFilesPath.Text := dlgWYRMFileSelector.FileName;
      memoWrymContents.Lines.LoadFromFile(edtScenarioFilesPath.Text);
      if(cmbModel.Text = CDDTS) then
      begin
        edtDDTSFilesPath.Text := dlgWYRMFileSelector.FileName;
        memoDDTSContents.Lines.LoadFromFile(edtDDTSFilesPath.Text);
      end;
      btnSave.Enabled := True;
      FScenarioHasChanged := True;
      CheckPathDosCompatibilty;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.FormShow(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.FormShow';
begin
  try
    memoWrymContents.Clear;
    memoDDTSContents.Clear;
    if(FFormActionState in [fasEdit,fasDelete]) then
    begin
      if (Trim(edtScenarioFilesPath.Text) <> '') and
         FileExists(edtScenarioFilesPath.Text) then
      memoWrymContents.Lines.LoadFromFile(edtScenarioFilesPath.Text);

      if(cmbModel.Text = CDDTS) then
      begin

        if (Trim(edtDDTSFilesPath.Text) <> '') and
          FileExists(edtDDTSFilesPath.Text) then
        memoDDTSContents.Lines.LoadFromFile(edtDDTSFilesPath.Text);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.chkbFilesLoadedClick(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.chkbFilesLoadedClick';
begin
  try
    FScenarioHasChanged := True;
    btnSave.Enabled := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.chkbStudyDataLockedClick(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.chkbStudyDataLockedClick';
begin
  try
    FScenarioHasChanged := True;
    btnSave.Enabled := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.cmbCalenderStartMonthChange(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.cmbCalenderStartMonthChange';
begin
  try
    FScenarioHasChanged := True;
    btnSave.Enabled := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmStudyEditForm.CheckPathDosCompatibilty: boolean;
const OPNAME = 'TfrmStudyEditForm.CheckPathDosCompatibilty';
begin
  Result := False;
  try
    Result := FilePathIsDosCompatible(FAppModules,edtScenarioFilesPath.Text);
    if not Result then
      ShowMessage(FAppModules.Language.GetString('FilePath.PathNotDosCompatible'));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.edtScenarioFilesPathExit(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.edtScenarioFilesPathExit';
begin
  try
    if CheckPathDosCompatibilty then
     edtScenarioExit(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.RunTabsheetsManager;
const OPNAME = 'TfrmStudyEditForm.RunTabsheetsManager';
begin
  try
    with pcModel do
    begin
      tbsYieldModel.TabVisible := (cmbModel.Text = CYield) or (cmbModel.Text = CPlanning);
      tbsHydrology.TabVisible  := (cmbModel.Text = CHydrology );
      tbsRainfall.TabVisible   := (cmbModel.Text = CRainfall );
      tbsStomsa.TabVisible     := (cmbModel.Text = CStomsa );
      tbsDDTS.TabVisible       := (cmbModel.Text = CDDTS);
    end;

    lblSubAreaCaption.Caption  := FAppModules.Language.GetString('StudySelection.SubAreaCaption');
    lblSubAreaLabelCaption.Caption := FAppModules.Language.GetString('StudySelection.SubAreaName');
    if (cmbModel.Text = CRainfall) then
    begin
      pnlScenario.Visible             := True;
      Self.Height                     := 551;
      edtScenario.Visible             := False;
      lblScenarioCaption.Visible      := False;
      edtScenarioLabel.Visible        := False;
      lblScenarioLabelCaption.Visible := False;
      cmbVersion.Visible              := False;
      lblVersion.Visible              := False;
      chkbFilesLoaded.Visible         := False;
      memoScenarioDescr.Visible       := False;
      lblScenarioDescr.Visible        := False;
      chkbStudyDataLocked.Visible     := True;
      chkbStudyDataLocked.Top         := 8;

      {pnlScenario.Visible := False;
      Self.Height := 393;
      }
      {
      lblScenarioCaption.Caption := FAppModules.Language.GetString('StudySelection.RainfallZone');
      lblScenarioLabelCaption.Caption := FAppModules.Language.GetString('StudySelection.ZoneName');
      lblVersion.Enabled := False;
      cmbVersion.Enabled := False;
      cmbVersion.Color := clBtnFace;
      }
    end
    else
    begin
      pnlScenario.Visible := True;
      Self.Height := 551;

      edtScenario.Visible             := True;
      lblScenarioCaption.Visible      := True;
      edtScenarioLabel.Visible        := True;
      lblScenarioLabelCaption.Visible := True;
      cmbVersion.Visible              := True;
      lblVersion.Visible              := True;
      chkbFilesLoaded.Visible         := True;
      memoScenarioDescr.Visible       := True;
      lblScenarioDescr.Visible        := True;
      chkbStudyDataLocked.Visible     := True;
      chkbStudyDataLocked.Top         := 125;

      lblScenarioCaption.Caption := FAppModules.Language.GetString('StudySelection.ScenarioCaption');
      lblScenarioLabelCaption.Caption := FAppModules.Language.GetString('StudySelection.ScenarioName');
      {lblVersion.Enabled := True;
      cmbVersion.Enabled := True;
      cmbVersion.Color := clWindow;}
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmStudyEditForm.GetModel: string;
const OPNAME = 'TfrmStudyEditForm.GetModel';
begin
  Result := '';
  try
    Result := FStudyFields.FModel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.btnSetDirectoryClick(Sender : TObject);
const OPNAME = 'TfrmStudyEditForm.btnSetDirectoryClick';
      SELDIRHELP = 1000;
var
  LDir,
  LOldDirFile : string;
begin
  try
    LOldDirFile :=  GetAppDataLocalDir+'\wrcdata\'; //ExtractFilePath(ApplicationExeName) + 'wrcdata\';
    if SelectDirectory(LDir,[sdAllowCreate,sdPerformCreate,sdPrompt], SELDIRHELP) then
    begin
      edtRainfallDefaultDirectory.Text := LDir;
      if (edtRainfallDefaultDirectory.Text[Length(edtRainfallDefaultDirectory.Text)] <> '\') then
        edtRainfallDefaultDirectory.Text := edtRainfallDefaultDirectory.Text+'\';
      btnSave.Enabled := True;
      FScenarioHasChanged := True;
      if (UpperCase(lOldDirFile)<>UpperCase(edtRainfallDefaultDirectory.Text)) then
      begin
        CopyClassRAndPatRUtiliesToNewFolder(LOldDirFile  + FAppModules.Language.GetString('Rainfall.Classr'), edtRainfallDefaultDirectory.Text + FAppModules.Language.GetString('Rainfall.Classr'));
        CopyClassRAndPatRUtiliesToNewFolder(LOldDirFile  + FAppModules.Language.GetString('Rainfall.Patchr'), edtRainfallDefaultDirectory.Text + FAppModules.Language.GetString('Rainfall.Patchr'));
        CopyClassRAndPatRUtiliesToNewFolder(LOldDirFile  + FAppModules.Language.GetString('Rainfall.HDYP08'), edtRainfallDefaultDirectory.Text + FAppModules.Language.GetString('Rainfall.HDYP08'));
        CopyClassRAndPatRUtiliesToNewFolder(LOldDirFile  + FAppModules.Language.GetString('Rainfall.tnt'), edtRainfallDefaultDirectory.Text + FAppModules.Language.GetString('Rainfall.tnt'));
        CopyClassRAndPatRUtiliesToNewFolder(LOldDirFile  + FAppModules.Language.GetString('Rainfall.LF90'), edtRainfallDefaultDirectory.Text + FAppModules.Language.GetString('Rainfall.LF90'));
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.CopyClassRAndPatRUtiliesToNewFolder(AOldDirFile,ANewDirFile : string );
const OPNAME = 'TfrmStudyEditForm.CopyClassRAndPatRUtiliesToNewFolder';
var
  LNewFile: TFileStream;
  LOldFile: TFileStream;
begin
  if (UpperCase(aOldDirFile) = UpperCase(aNewDirFile)) then
    Exit;
  try
    if (FileExists(aNewDirFile)) then
      DeleteFile(aNewDirFile);
    LOldFile := TFileStream.Create(AOldDirFile,fmOpenRead or fmShareDenyWrite);
    try
      LNewFile := TFileStream.Create(ANewDirFile,fmCreate or fmShareExclusive);
      try
        LNewFile.CopyFrom(LOldFile,LOldFile.Size);
      finally
        FreeAndNil(LNewFile);
      end;
    finally
      FreeAndNil(LOldFile);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.cmbVersionChange(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.cmbVersionChange';
begin
  try
    if(cmbModel.Text = CYield) and (cmbVersion.Text <> '7')then
      ShowMessage('The selected scenario version is no longer supported. Only version 7 is used.');
    if(NodeLevel = nlScenario) then
      edtScenarioExit(Sender);
    UpdateStudyVersionDescriptionLabel;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.btnScenarioPathCreateClick(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.btnScenarioPathCreateClick';
begin
  try
    if not Assigned(FWrymForm) then
      FWrymForm := TfrmCreateWrymForm.Create(Self,FAppModules);
    FWrymForm.FileName := edtScenarioFilesPath.Text;
    FWrymForm.ModelName    := Trim(cmbModel.Text);
    if(cmbModel.Text = CPlanning) then
      FWrymForm.ModelVersion := '7'
    else
      FWrymForm.ModelVersion := Trim(cmbVersion.Text);
    if(cmbModel.Text = CYield) then
      FWrymForm.Caption := 'Create WRYM.dat file';
    if(cmbModel.Text = CPlanning) then
      FWrymForm.Caption := 'Create WRPM.dat file';
     if(cmbModel.Text = CDDTS) then
      FWrymForm.Caption := 'Create DDTS.dat file';
    FWrymForm.ShowModal;
    if(FWrymForm.ModalResult = mrOk) then
    begin
      edtScenarioFilesPath.Text := FWrymForm.FileName;
      edtScenarioFilesPathExit(edtScenarioFilesPath);
      memoWrymContents.Lines.LoadFromFile(edtScenarioFilesPath.Text);
      if(cmbModel.Text = CDDTS) then
      begin
        edtDDTSFilesPath.Text := FWrymForm.FileName;
        edtScenarioFilesPathExit(edtDDTSFilesPath);
        memoDDTSContents.Lines.LoadFromFile(edtDDTSFilesPath.Text);

      end;

      btnSave.Enabled := True;
      FScenarioHasChanged := True;
      CheckPathDosCompatibilty;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.SelectStudyDataForModel(AStudyFields: TStudyFields);
const OPNAME = 'TfrmStudyEditForm.SelectStudyDataForModel';
var
  LDataSet : TAbstractModelDataset;
  LSQL: string;
begin
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), LDataSet);
    try
      LSQL := 'SELECT Model,StudyAreaName,StudyDate,Consultant,Client,StudyNumber,'+
              ' StudyLabel,StudyAreaDescr FROM StudyArea'+
              ' WHERE StudyAreaName = '+ QuotedStr(AStudyFields.FStudyAreaName) +
              ' AND Model = '+ QuotedStr(AStudyFields.FModel);
      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;

      //Check if there is any data.
      if not (LDataSet.DataSet.Eof  and LDataSet.DataSet.Bof) then
      begin
        LDataSet.DataSet.First;
        AStudyFields.FStudyDate := LDataSet.DataSet.FieldByName('StudyDate').AsDateTime;
        AStudyFields.FConsultant := Trim(LDataSet.DataSet.FieldByName('Consultant').AsString);
        AStudyFields.FClient := Trim(LDataSet.DataSet.FieldByName('Client').AsString);
        AStudyFields.FStudyNumber := Trim(LDataSet.DataSet.FieldByName('StudyNumber').AsString);
        AStudyFields.FStudyLabel := Trim(LDataSet.DataSet.FieldByName('StudyLabel').AsString);
        AStudyFields.FStudyAreaDescr := Trim(LDataSet.DataSet.FieldByName('StudyAreaDescr').AsString);
      end;
    finally
      LDataSet.Free;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.FormKeyPress(Sender: TObject; var Key: Char);
const OPNAME = 'TfrmStudyEditForm.FormKeyPress';
begin
  try
    if (Key = '_') then
    begin
      if (Sender = edtStudy) or (Sender = edtSubArea) or (Sender = edtScenario) then
      begin
        Key := #0;
        Beep;
      end;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
procedure TfrmStudyEditForm.btnLoadStudyShapeFilesClick(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.btnLoadStudyShapeFilesClick';
var
  LSaveDialog : TSaveDialog;
begin
  try
    try
      LSaveDialog := TSaveDialog.Create(nil);
      LSaveDialog.DefaultExt  := 'SHP';
      LSaveDialog.Filter      := FAppModules.Language.GetString('StudySelection.ShapeFilesFilter');
      LSaveDialog.FilterIndex := 1;
      if (LSaveDialog.Execute) then
        if (RelatedFilesFound(LSaveDialog.FileName)) then
          if (Sender = btnLoadStudyShapeFiles)  then
          begin
            edtStudyShapeFileName.Text := Copy(ExtractFileName(LSaveDialog.FileName), 1, Length(ExtractFileName(LSaveDialog.FileName)) - 4);
            if not (FFormActionState = fasAdd) then
            begin
              btnSave.Enabled := True;
              FStudyHasChanged := True;
            end;
          end
          else
          if (Sender = btnLoadSubAreaShapeFiles)  then
          begin
            edtSubAreaShapeFileName.Text := Copy(ExtractFileName(LSaveDialog.FileName),1,Length(ExtractFileName(LSaveDialog.FileName))-4);
            if not (FFormActionState = fasAdd) then
            begin
              btnSave.Enabled := True;
              FSubAreHasChanged := True;
            end;  
          end;
    finally
      FreeAndNil(LSaveDialog);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfrmStudyEditForm.RelatedFilesFound(ASelectedFile : string): boolean;
const OPNAME = 'TfrmStudyEditForm.RelatedFilesFound';
var
  LSearchFile : TSearchRec;
  LFileName : string;
begin
  Result := False;
  LFileName := Copy(ASelectedFile,1,Length(ASelectedFile)-4);
  try
    Result := (FindFirst(LFileName +'.shx',faAnyFile,LSearchFile) = 0);
    Result := Result and (FindFirst(LFileName + '.dbf',faAnyFile,LSearchFile) = 0);
    Result := Result or (FindFirst(LFileName + '.sbx',faAnyFile,LSearchFile) = 0);
    Result := Result or (FindFirst(LFileName + '.sbn',faAnyFile,LSearchFile) = 0);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.DoOnKeypress(Sender: TObject; var Key : Char);
const OPNAME = 'TfrmStudyEditForm.DoOnKeypress';
begin
  try
    if ValidateIndexFields then
    begin
      case FNodeLevel of
        nlStudy:
        begin
          FStudyHasChanged := True;
          btnSave.Enabled := True;
        end;
        nlModel:
        begin
          FStudyHasChanged := True;
          btnSave.Enabled := True;
        end;
        nlSubArea:
        begin
          FSubAreHasChanged := True;
          btnSave.Enabled := True;
        end;
        nlScenario:
        begin
          FScenarioHasChanged := True;
          btnSave.Enabled := True;
        end;
      end;
    end
    else
      btnSave.Enabled := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
{
function TfrmStudyEditForm.OnHelpRequest(Command: Word;Data: THelpEventData; var CallHelp: Boolean): Boolean;
const OPNAME = 'TfrmStudyEditForm.OnHelpRequest';
begin
  Result := FALSE;
  try
    if(Data = HC_WaterResourcesYieldModel) or (Data = HC_RunControl) or
      (Data = HC_RunDescription)           or (Data = HC_DataFileLocation) or
      (Data = HC_AnalysisPeriod) then
    Application.OnHelp(Command,Data,CallHelp);
    //Application.HelpContext(Data);

    CallHelp := True;
    Result   := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
}

procedure TfrmStudyEditForm.PopulateModelVersion;
const OPNAME = 'TfrmStudyEditForm.PopulateModelVersion';
begin
  try
    if(cmbModel.Text = CYield) then
    begin
      cmbVersion.Items.CommaText := '6,6.1,6.2,7';
      cmbVersion.ItemIndex       := cmbVersion.Items.Count-1;
      cmbVersion.Text            := cmbVersion.Items[cmbVersion.ItemIndex];
      UpdateStudyVersionDescriptionLabel;
    end
    else if (cmbModel.Text = CPlanning) then
    begin
      cmbVersion.Items.CommaText := 'WRPM_443r.exe';
      cmbVersion.ItemIndex       := 0;
      cmbVersion.Text            := cmbVersion.Items[cmbVersion.ItemIndex];
      UpdateStudyVersionDescriptionLabel;
    end
    else if(cmbModel.Text = CStomsa) then
    begin
      cmbVersion.Items.CommaText := '0.3';
      cmbVersion.ItemIndex       := 0;
      cmbVersion.Text            := cmbVersion.Items[cmbVersion.ItemIndex];
      UpdateStudyVersionDescriptionLabel;
    end
    else
    begin
      cmbVersion.Items.CommaText := '1';
      cmbVersion.ItemIndex       := 0;
      cmbVersion.Text            := cmbVersion.Items[cmbVersion.ItemIndex];
      UpdateStudyVersionDescriptionLabel;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.UpdateStudyVersionDescriptionLabel;
const OPNAME = 'TfrmStudyEditForm.UpdateStudyVersionDescriptionLabel';
begin
  try
    lblVersionDescr.Caption := '';
    if(cmbModel.Text = CYield) then
    begin
      case cmbVersion.ItemIndex of
        0: lblVersionDescr.Caption := '(Yield model)';//6
        1: lblVersionDescr.Caption := '(Yield model with Calculate Historic Firm Yield option 2)';//6.1
        2: lblVersionDescr.Caption := '(Yield model with Demand Reconciliation)';//6.2
        3: lblVersionDescr.Caption := '(Yield model with Irrigation blocks)'; //7
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
begin

end;

{procedure TfrmStudyEditForm.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
const OPNAME = 'TfrmStudyEditForm.FormCanResize';
var
  LDefaultWidth : Integer;
begin
  try
    LDefaultWidth := btnSave.ClientWidth + btnReset.ClientWidth + btnCancel.ClientWidth +
                     ProgressBar1.ClientWidth + 160;


    if (NewWidth < LDefaultWidth ) then   //565
      Resize := false;

    if (NewHeight <  520) then
      Resize := false;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;}

procedure TfrmStudyEditForm.btnStomsaHydrologypathClick(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.btnStomsaHydrologypathClick';
var
  LDir : string;
begin
  try
    if SelectDirectory(LDir,[sdAllowCreate,sdPerformCreate,sdPrompt], 0) then
    begin
      edtStomsaHydrologypath.Text := LDir;
      btnSave.Enabled := True;
      FScenarioHasChanged := True;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.SetModelLabels(AModelName: string);
const OPNAME = 'TfrmStudyEditForm.SetModelLabels';
begin
  try
    lblMaxChars.Visible := False;
    if(AModelName = CYield) then
      tbsYieldModel.Caption    := FAppModules.Language.GetString('TabCaption.Yield')
    else if (AModelName = CPlanning) then
      tbsYieldModel.Caption    := FAppModules.Language.GetString('TabCaption.Planning');

    if(AModelName = CYield) then
    begin
      lblScenarioFilesPathCaption.Caption := FAppModules.Language.GetString('LabelCaption.WrymDatFile');
      lblWrymFileContents.Caption         := FAppModules.Language.GetString('LabelCaption.WrymFileData');
    end
    else if(AModelName = CPlanning) then
    begin
      lblScenarioFilesPathCaption.Caption := FAppModules.Language.GetString('LabelCaption.WrypDatFile');
      lblWrymFileContents.Caption         := FAppModules.Language.GetString('LabelCaption.WrypFileData');
    end
    else if(AModelName = CHydrology) then
    begin
      lblScenarioCaption.Caption       := 'Network Code';
      lblScenarioLabelCaption.Caption  := 'Code Descr';
      chkbStudyDataLocked.Caption      := 'Read Only';
      lblMaxChars.Visible              := True;
    end
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.edtScenarioChange(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.edtScenarioChange';
begin
  try
    if(cmbModel.Text = CHydrology) then
    begin
      btnSave.Enabled     := ValidateIndexFields;
      FScenarioHasChanged := btnSave.Enabled;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfrmStudyEditForm.btnHydrologyIInputDirClick(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.btnHydrologyIInputDirClick';
var
  LDir : String;
begin
  try
    LDir := ExtractFilePath(Application.ExeName);
    if SelectDirectory(LDir, [sdAllowCreate, sdPerformCreate, sdPrompt],0) then
      edtHydrologyInputDir.Text := LDir;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyEditForm.btnHydrologyIOuputDirClick(Sender: TObject);
const OPNAME = 'TfrmStudyEditForm.btnHydrologyIOuputDirClick';
var
  LDir : String;
begin
  try
    LDir := ExtractFilePath(Application.ExeName);
    if SelectDirectory(LDir, [sdAllowCreate, sdPerformCreate, sdPrompt],0) then
      edtHydrologyIOutputDir.Text := LDir;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TfrmStudyEditForm.PopulateHydrologyScenario(AStudyFields: TStudyFields);
const OPNAME = 'TfrmStudyEditForm.PopulateHydrologyScenario';
var
  LHydroDBManager     : THydroDBManager;
  LValues             : string;
  LNetworkProperties  : TStringList;
begin
  try
    if(AStudyFields.FSubModel <> CHydrology) then
      Exit;
    LHydroDBManager     := THydroDBManager.Create(FAppModules);
    LNetworkProperties  := TStringList.Create;
    try
      LValues := LHydroDBManager.Get_NetworkPropertiesCommText(AStudyFields.FCalenderStartMonth);
      LNetworkProperties.CommaText := LValues;
      if(LNetworkProperties.Values['NetworkID'] <> '') then
        edtNetworkID.Text := LNetworkProperties.Values['NetworkID'];
      if(LNetworkProperties.Values['NetworkCode'] <> '') then
        edtNetworkCode.Text := LNetworkProperties.Values['NetworkCode'];
      if(LNetworkProperties.Values['VersionNo'] <> '') then
        cmbVersion.ItemIndex := cmbVersion.Items.IndexOf(LNetworkProperties.Values['VersionNo']);
      if(LNetworkProperties.Values['InputDirectory'] <> '') then
        edtHydrologyInputDir.Text := LNetworkProperties.Values['InputDirectory'];
      if(LNetworkProperties.Values['OutputDirectory'] <> '') then
        edtHydrologyIOutputDir.Text := LNetworkProperties.Values['OutputDirectory'];
      if(LNetworkProperties.Values['DebugRequired'] <> '') then
        chkHydrologyIDebugRequired.Checked := LNetworkProperties.Values['DebugRequired'] = 'Y';
      if(LNetworkProperties.Values['DebugStartPeriod'] <> '') then
        edtHydrologyIDebugStart.Text := LNetworkProperties.Values['DebugStartPeriod'];
      if(LNetworkProperties.Values['DebugEndPeriod'] <> '') then
        edtHydrologyIDebugEnd.Text := LNetworkProperties.Values['DebugEndPeriod'];
      if(LNetworkProperties.Values['SummaryRequired'] <> '') then
        chkHydrologyISumRequired.Checked := LNetworkProperties.Values['SummaryRequired'] = 'Y';
      if(LNetworkProperties.Values['SimulationStartYear'] <> '') then
        edtHydrologyISimStart.Text := LNetworkProperties.Values['SimulationStartYear'];
      if(LNetworkProperties.Values['SimulationEndYear'] <> '') then
        edtHydrologyISimEnd.Text := LNetworkProperties.Values['SimulationEndYear'];
      if(LNetworkProperties.Values['IsReadOnly'] <> '') then
        chkbStudyDataLocked.Checked := LNetworkProperties.Values['IsReadOnly']='1';
    finally
      LHydroDBManager.Free;
      LNetworkProperties.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

function TfrmStudyEditForm.Get_Set_NetworkPropertiesCommText: string;
const OPNAME = 'THydroDBManager.Get_NetworkPropertiesCommText';
var
  LNetworkProperties   : TStringList;
begin
  Result := '';
  try
    LNetworkProperties := TStringList.Create;
    try
      LNetworkProperties.Add('NetworkID='+edtNetworkID.Text);
      LNetworkProperties.Add('NetworkCode='+edtNetworkCode.Text);
      LNetworkProperties.Add('VersionNo='+cmbVersion.Text);
      LNetworkProperties.Add('InputDirectory='+edtHydrologyInputDir.Text);
      LNetworkProperties.Add('OutputDirectory='+edtHydrologyIOutputDir.Text);
      LNetworkProperties.Add('DebugStartPeriod='+edtHydrologyIDebugStart.Text);
      LNetworkProperties.Add('DebugEndPeriod='+edtHydrologyIDebugEnd.Text);
      LNetworkProperties.Add('SimulationStartYear='+edtHydrologyISimStart.Text);
      LNetworkProperties.Add('SimulationEndYear='+edtHydrologyISimEnd.Text);
      LNetworkProperties.Add('MinLongitude=');
      LNetworkProperties.Add('MaxLongitude=');
      LNetworkProperties.Add('MinLatitude=');
      LNetworkProperties.Add('MaxLatitude=');
      if chkbStudyDataLocked.Checked then
        LNetworkProperties.Add('IsReadOnly=1')
      else
        LNetworkProperties.Add('IsReadOnly=0');
      if chkHydrologyIDebugRequired.Checked then
        LNetworkProperties.Add('DebugRequired=Y')
      else
        LNetworkProperties.Add('DebugRequired=N');
      if chkHydrologyISumRequired.Checked then
        LNetworkProperties.Add('SummaryRequired=Y')
      else
        LNetworkProperties.Add('SummaryRequired=N');
      Result := LNetworkProperties.CommaText;
    finally
      LNetworkProperties.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
