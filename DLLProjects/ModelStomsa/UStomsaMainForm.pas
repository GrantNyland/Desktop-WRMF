unit UStomsaMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.ExtCtrls, VCL.Menus, VCL.StdCtrls, VCL.ActnList,VCL.ComCtrls,
{$WARN UNIT_PLATFORM OFF}
  VCL.FileCtrl,
{$WARN UNIT_PLATFORM ON}
  UHotBtn,
  UAbstractObject,
  UGenericModelLinkClasses;

type
  TfmStomsaMainForm = class(TFrame)
    pnlMainMenu: TPanel;
    btnProjectOpen: THotBtn;
    btnExit: THotBtn;
    btnProjectClose: THotBtn;
    btnProjectSave: THotBtn;
    btnProjectNew: THotBtn;
    dlgSaveProjectFiles: TSaveDialog;
    dlgProjectOpen: TOpenDialog;
    pnlStatus: TPanel;
    lblProjectDescription: TLabel;
    shpStatus: TShape;
    btnProjectMerge: THotBtn;
    dlgSavePARAMFiles: TSaveDialog;
    dlgOpenParamFile: TOpenDialog;
    btnOpenParam: THotBtn;
    btnHelp: THotBtn;
    dlgSaveStochasticFiles: TSaveDialog;
    Bevel7: TBevel;
    Bevel8: TBevel;
    pnlLeft: TPanel;
    pnlTaskBar: TPanel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Panel1: TPanel;
    imgSelectDone: TImage;
    btnSelectInc: THotBtn;
    Panel2: TPanel;
    imgKeyGaugesDone: TImage;
    btnKeyGauges: THotBtn;
    Panel3: TPanel;
    imgMarginalFitted: TImage;
    btnMarginal: THotBtn;
    Panel4: TPanel;
    imgTimeSeriesFitted: TImage;
    btnTimeSeries: THotBtn;
    Panel5: TPanel;
    imgPARAMFileCreated: TImage;
    btnParamFiles: THotBtn;
    Panel6: TPanel;
    imgGenerated: TImage;
    btnGenerate: THotBtn;
    Panel7: TPanel;
    btnOutputManager: THotBtn;
    Panel8: TPanel;
    btnRunAutomatic: THotBtn;
    Panel9: TPanel;
    imgPARAMFileSaved: TImage;
    btnSavePARAMFile: THotBtn;
    btnGraphs: THotBtn;
    Panel13: TPanel;
    imgCorrelated: TImage;
    btnCorrelate: THotBtn;
    procedure btnSelectIncClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure btnGraphsClick(Sender: TObject);
    procedure btnCorrelateClick(Sender: TObject);
    procedure btnMarginalClick(Sender: TObject);
    procedure btnTimeSeriesClick(Sender: TObject);
    procedure btnParamFilesClick(Sender: TObject);
    procedure btnSavePARAMFileClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnRunAutomaticClick(Sender: TObject);
    procedure btnKeyGaugesClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnOutputManagerClick(Sender: TObject);
  protected
    FAppModules: TAppModules;
    FCurrentFrame: TFrame;
    procedure WriteANSFile;
    procedure ClearFarames;
  public
    { Public declarations }
    procedure AfterConstruction; override;
    procedure AddFrame(AFrame:TFrame);
    function OnFileNew : boolean;
    function OnFileOpen : boolean;
    function OnFileOpenParam : boolean;
    function OnFileSave : boolean;
    function OnFileSaveAs : boolean;
    function OnFileSaveANS : boolean;
    function OnFileMerge : boolean;
    function OnFileClose : boolean;
    constructor Create(AOwner: TComponent; AAppModules: TAppModules); reintroduce; virtual;
    function ResetState: boolean; virtual;
    function Initialise: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function StudyHasChanged: boolean; virtual;
    function DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;virtual;
  end;

var
  fmStomsaMainForm: TfmStomsaMainForm;

implementation

uses
  UStomsaGlobalData,
  USelectIncForm,
  UMarginalForm,
  UDataModule,
  UTimeSeriesForm,
  UParamResultsForm,
  UKeyGaugesForm,
  UAboutForm,
  UGenerateForm,
  UOutputManagerForm,
  UErrorForm,
  UMessagesForm,
  UGraphForm,
  UCorrelateForm,
  UMainMenuEventType,
  UErrorHandlingOperations;

{$R *.DFM}

constructor TfmStomsaMainForm.Create(AOwner: TComponent;AAppModules: TAppModules);
const OPNAME = 'TfmStomsaMainForm.Create';
begin
  inherited Create(AOwner);
  try
    FAppModules := AAppModules;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.AfterConstruction;
const OPNAME = 'TfmStomsaMainForm.AfterConstruction';
begin
  inherited;
  try
    FCurrentFrame := nil;
    dlgSaveProjectFiles.InitialDir := FAppModules.StudyArea.DataFilesPath;
    dlgSavePARAMFiles.InitialDir := FAppModules.StudyArea.DataFilesPath;
    dlgSaveStochasticFiles.InitialDir := FAppModules.StudyArea.DataFilesPath;
    dlgProjectOpen.InitialDir := FAppModules.StudyArea.DataFilesPath;
    dlgOpenParamFile.InitialDir := FAppModules.StudyArea.DataFilesPath;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.AddFrame(AFrame: TFrame);
const OPNAME = 'TfmStomsaMainForm.AddFrame';
begin
  try
    if (FCurrentFrame <> nil) then
    begin
      FCurrentFrame.Visible := False;
      FCurrentFrame.Parent  := nil;
      FCurrentFrame         := nil;
    end;
    if (AFrame <> nil) then
    begin
      FCurrentFrame         := AFrame;
      FCurrentFrame.Parent  := Self;
      FCurrentFrame.Align   := alClient;
      FCurrentFrame.Visible := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnExitClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnExitClick';
begin
  try
    fmMessages.DisplayMessage('Cleaning up data');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//Left buttons event handlers
procedure TfmStomsaMainForm.btnSelectIncClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnSelectIncClick';
begin
  try
    if fmSelectInc = nil then
    begin
      fmSelectInc := TfmSelectInc.Create(Self,FAppModules);
    end;
    AddFrame(fmSelectInc);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnKeyGaugesClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnKeyGaugesClick';
begin
  try
    if fmKeyGauges = nil then
    begin
      fmKeyGauges := TfmKeyGauges.Create(Self,FAppModules);
    end;
    AddFrame(fmKeyGauges);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnMarginalClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnMarginalClick';
begin
  try
    if fmMarginal = nil then
    begin
      fmMarginal := TfmMarginal.Create(Self, FAppModules);
    end;
    AddFrame(fmMarginal);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnTimeSeriesClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnTimeSeriesClick';
begin
  try
    if fmTimeSeries = nil then
    begin
      fmTimeSeries := TfmTimeSeries.Create(Self,FAppModules);
    end;
    AddFrame(fmTimeSeries);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnParamFilesClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnParamFilesClick';
begin
  try
    if fmPARAMResults = nil then
    begin
      fmPARAMResults := TfmPARAMResults.Create(Self,FAppModules);
    end;
    AddFrame(fmPARAMResults);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnGenerateClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnGenerateClick';
begin
  try
    if fmGenerate = nil then
    begin
      fmGenerate := TfmGenerate.Create(nil,FAppModules);
    end;
    AddFrame(fmGenerate);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnCorrelateClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnCorrelateClick';
begin
  try
    if fmCorrelate = nil then
    begin
      fmCorrelate := TfmCorrelate.Create(Self,FAppModules);
    end;
    AddFrame(fmCorrelate);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnGraphsClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnGraphsClick';
begin
 try
    if fmGraph = nil then
    begin
      fmGraph := TfmGraph.Create(Self,FAppModules);
    end;
    AddFrame(fmGraph);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.About1Click(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.About1Click';
begin
  try
    if fmAbout = nil then
    begin
      fmAbout := TfmAbout.Create(Self,FAppModules);
    end;
    AddFrame(fmAbout);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnOutputManagerClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnOutputManagerClick';
begin
  try
    if fmOutputManager = nil then
    begin
      fmOutputManager := TfmOutputManager.Create(Self,FAppModules);
    end;
    AddFrame(fmOutputManager);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnSavePARAMFileClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnSavePARAMFileClick';
begin
  try
    if NOT(fmData.DataStorage.Automatic) then
    begin
      if dlgSavePARAMFiles.Execute then
      begin
        if fmData.DataStorage.SaveParamFile(dlgSavePARAMFiles.FileName) then
          imgPARAMFileSaved.Show;
      end;
    end
    else
    begin
      if fmData.DataStorage.SavePARAMFile('PARAM.DAT') then
        imgPARAMFileSaved.Show;
    end;
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.btnRunAutomaticClick(Sender: TObject);
const OPNAME = 'TfmStomsaMainForm.btnRunAutomaticClick';
begin
  try
    fmData.DataStorage.Automatic := true;
    btnMarginalClick(nil);
    btnTimeSeriesClick(nil);
    btnParamFilesClick(nil);
    btnSavePARAMFileClick(nil);
    btnGenerateClick(nil);
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfmStomsaMainForm.WriteANSFile;
const OPNAME = 'TfmStomsaMainForm.WriteANSFile';
var
  LFileName : string;
begin
  try
    with fmData.DataStorage.CurrentRecord do
    begin
      if MarginalFitted then
      begin
        LFileName := IncludeTrailingPathDelimiter(Directory) + FileName;
        LFileName := ChangeFileExt(LFileName,'.ans');
        fmData.DataStorage.WriteANSFile(LFileName);
      end;
    end;
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfmStomsaMainForm.OnFileNew : boolean;
const OPNAME = 'TfmStomsaMainForm.OnFileNew';
begin
  Result := False;
  try
    ProjectName                   := '<New>';
    lblProjectDescription.Caption := ProjectName;
    btnSelectInc.Enabled          := True;

    shpStatus.Brush.Color :=  clGreen;
    fmData.DataStorage.RefreshInc;
    fmData.DataStorage.Initialise;
    btnSelectIncClick(nil);
    pnlTaskBar.Show;
    Result := True;
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfmStomsaMainForm.OnFileOpen : boolean;
const OPNAME = 'TfmStomsaMainForm.OnFileOpen';
begin
  Result := False;
  try
    dlgProjectOpen.InitialDir := FAppModules.StudyArea.DataFilesPath;
    if dlgProjectOpen.Execute then
    begin
      if fmData.DataStorage.OpenProject(dlgProjectOpen.FileName) then
      begin
        ProjectName                   := dlgProjectOpen.FileName;
        lblProjectDescription.Caption := ProjectName;
        shpStatus.Brush.Color         := clGreen;
        btnSelectInc.Enabled          := True;

        btnSelectIncClick(nil);
        pnlTaskBar.Show;
        Result := True;
      end;
    end;
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfmStomsaMainForm.OnFileOpenParam : boolean;
const OPNAME = 'TfmStomsaMainForm.OnFileOpenParam';
begin
  Result := False;
  try
    if dlgOpenParamFile.Execute then
    begin
      if fmData.DataStorage.OpenPARAMFile(dlgOpenParamFile.FileName) then
      begin
        lblProjectDescription.Caption := dlgOpenParamFile.FileName;
        btnSelectInc.Enabled          := False;
        Result := True;
      end;
    end;
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfmStomsaMainForm.OnFileSave : boolean;
const OPNAME = 'TfmStomsaMainForm.OnFileSave';
begin
  Result := False;
  try
    if ProjectName <> '<New>' then
      dlgSaveProjectFiles.FileName := ProjectName;

    if dlgSaveProjectFiles.Execute then
    begin
      ProjectName := dlgSaveProjectFiles.FileName;
      lblProjectDescription.Caption := ProjectName;
      fmData.DataStorage.SaveProject(ProjectName);
      Result := True;
    end;
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfmStomsaMainForm.OnFileSaveANS : boolean;
const OPNAME = 'TfmStomsaMainForm.OnFileSaveANS';
begin
  Result := False;
  try
    if fmData.DataStorage.First then
      WriteANSFile;
    while fmData.DataStorage.Next do
      WriteANSFile;
    Result := True;
    FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfmStomsaMainForm.OnFileSaveAs : boolean;
const OPNAME = 'TfmStomsaMainForm.OnFileSaveAs';
begin
  Result := False;
  try
    Result := OnFileSaveANS;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfmStomsaMainForm.OnFileClose : boolean;
const OPNAME = 'TfmStomsaMainForm.OnFileClose';
begin
  Result := False;
  try
    FCurrentFrame := nil;

    ProjectName := '<None>';
    lblProjectDescription.Caption := ProjectName;

    btnSelectInc.Enabled      := false;
    btnKeyGauges.Enabled      := false;
    btnMarginal.Enabled       := False;
    btnTimeSeries.Enabled     := false;
    btnParamFiles.Enabled     := false;
    btnSavePARAMFile.Enabled  := False;
    btnGenerate.Enabled       := false;
    btnCorrelate.Enabled      := false;
    btnOutputManager.Enabled  := false;
    btnRunAutomatic.Enabled   := false;

    pnlTaskBar.Hide;
    imgSelectDone.Hide;
    fmSelectInc.Free;
    fmSelectInc := nil;
    fmKeyGauges.Free;
    fmKeyGauges := nil;
    imgKeyGaugesDone.Hide;
    fmMarginal.Free;
    fmMarginal := nil;
    imgMarginalFitted.Hide;
    fmTimeSeries.Free;
    fmTimeSeries := nil;
    imgTimeSeriesFitted.Hide;
    fmPARAMResults.Free;
    fmPARAMResults := nil;
    imgPARAMFileCreated.Hide;
    imgPARAMFileSaved.Hide;
    fmGenerate.Free;
    fmGenerate := nil;
    imgGenerated.Hide;
    fmCorrelate.Free;
    fmCorrelate := nil;
    imgCorrelated.Hide;
    fmOutputManager.Free;
    fmOutputManager := nil;
    fmErrorReporting.Free;
    fmErrorReporting := nil;
    shpStatus.Brush.Color := clBtnFace;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function  TfmStomsaMainForm.OnFileMerge : boolean;
const OPNAME = 'TfmStomsaMainForm.OnFileMerge';
var
  TheDirectory : string;
begin
  Result := False;
  try
  GetDir(0,TheDirectory);
  dlgProjectOpen.InitialDir := TheDirectory;
  if dlgProjectOpen.Execute then
  begin
    if fmData.DataStorage.MergeProject(dlgProjectOpen.FileName) then
    begin
      btnSelectIncClick(nil);
      Result := True;
    end;
  end;
  FAppModules.Model.ProcessEvent(CmeRefreshMenuItems,nil)
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfmStomsaMainForm.DoCustomTabSheetEvent(ACustomModelEvent: TModelMenuData): Boolean;
const OPNAME = 'TfmStomsaMainForm.DoCustomTabSheetEvent';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmStomsaMainForm.Initialise: boolean;
const OPNAME = 'TfmStomsaMainForm.Initialise';
begin
  Result := False;
  try
    ClearFarames;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmStomsaMainForm.LanguageHasChanged: boolean;
const OPNAME = 'TfmStomsaMainForm.LanguageHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmStomsaMainForm.ResetState: boolean;
const OPNAME = 'TfmStomsaMainForm.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmStomsaMainForm.StudyHasChanged: boolean;
const OPNAME = 'TfmStomsaMainForm.StudyHasChanged';
begin
  Result := False;
  try
    ClearFarames;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TfmStomsaMainForm.ClearFarames;
const OPNAME = 'TfmStomsaMainForm.ClearFarames';
begin
  try
    FCurrentFrame := nil;

    if(fmCorrelate <> nil) then
    begin
      fmCorrelate.Parent := nil;
      FreeAndNil(fmCorrelate);
    end;
    if(fmSelectInc <> nil) then
    begin
      fmSelectInc.Parent := nil;
      FreeAndNil(fmSelectInc);
    end;
    if(fmKeyGauges <> nil) then
    begin
      fmKeyGauges.Parent := nil;
      FreeAndNil(fmKeyGauges);
    end;
    if(fmMarginal <> nil) then
    begin
      fmMarginal.Parent := nil;
      FreeAndNil(fmMarginal);
    end;
    if(fmTimeSeries <> nil) then
    begin
      fmTimeSeries.Parent := nil;
      FreeAndNil(fmTimeSeries);
    end;
    if(fmPARAMResults <> nil) then
    begin
      fmPARAMResults.Parent := nil;
      FreeAndNil(fmPARAMResults);
    end;
    if(fmGenerate <> nil) then
    begin
      fmGenerate.Parent := nil;
      FreeAndNil(fmGenerate);
    end;
    if(fmGraph <> nil) then
    begin
      fmGraph.Parent := nil;
      FreeAndNil(fmGraph);
    end;
    if(fmAbout <> nil) then
    begin
      fmAbout.Parent := nil;
      FreeAndNil(fmAbout);
    end;
    if(fmOutputManager <> nil) then
    begin
      fmOutputManager.Parent := nil;
      FreeAndNil(fmOutputManager);
    end;

    OnFileClose;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
