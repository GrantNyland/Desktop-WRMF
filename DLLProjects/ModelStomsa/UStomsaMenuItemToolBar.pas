//
//
//  UNIT      : Contains TIFRMenuItemToolBar Class
//  AUTHOR    : Dziedzi Ramulondi (arivia.kom)
//  DATE      : 09/01/2007
//  COPYRIGHT : Copyright © 2007 DWAF
//
//

unit UStomsaMenuItemToolBar;


interface

uses
  UAbstractComponent,
  UHelpContexts,
  UChildToolBar;

type
  TStomsaMenuItemToolBar = class(TChildToolBar)
  protected
    FStomsaFileNewBtn       : TAbstractSpeedButton;
    FStomsaFileOpenBtn      : TAbstractSpeedButton;
    FStomsaFileOpenParamBtn : TAbstractSpeedButton;
    FStomsaFileSaveBtn      : TAbstractSpeedButton;
    FStomsaFileMergeBtn     : TAbstractSpeedButton;
    FStomsaFileCloseBtn     : TAbstractSpeedButton;
    FStomsaFileExportBtn    : TAbstractSpeedButton;
    FStomsaFileImportBtn    : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnFileNewBtnClick(Sender: TObject);
    procedure OnFileOpenParamBtnClick(Sender: TObject);
    procedure OnFileOpenBtnClick(Sender: TObject);
    procedure OnFileSaveBtnClick(Sender: TObject);
    procedure OnFileMergeBtnClick(Sender: TObject);
    procedure OnFileCloseBtnClick(Sender: TObject);
    procedure OnFileExportBtnClick(Sender: TObject);
    procedure OnFileImportBtnClick(Sender: TObject);
  public
    function LanguageHasChanged: boolean; override;
    procedure SetStomsaFileNew(AEnabled: boolean);
    procedure SetStomsaFileOpen(AEnabled: boolean);
    procedure SetStomsaFileOpenParam(AEnabled: boolean);
    procedure SetStomsaFileSave(AEnabled: boolean);
    procedure SetStomsaFileMerge(AEnabled: boolean);
    procedure SetStomsaFileClose(AEnabled: boolean);
    procedure SetStomsaFileImport(AEnabled: boolean);
    procedure SetStomsaFileExport(AEnabled: boolean);

    property StomsaFileNewBtn       : TAbstractSpeedButton read FStomsaFileNewBtn;
    property StomsaFileOpenBtn      : TAbstractSpeedButton read FStomsaFileOpenBtn;
    property StomsaFileOpenParamBtn : TAbstractSpeedButton read FStomsaFileOpenBtn;
    property StomsaFileSaveBtn      : TAbstractSpeedButton read FStomsaFileSaveBtn;
    property StomsaFileMergeBtn     : TAbstractSpeedButton read FStomsaFileMergeBtn;
    property StomsaFileCloseBtn     : TAbstractSpeedButton read FStomsaFileCloseBtn;
    property StomsaFileExportBtn    : TAbstractSpeedButton read FStomsaFileExportBtn;
    property StomsaFileImportBtn    : TAbstractSpeedButton read FStomsaFileImportBtn;
  end;

implementation

uses
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TStomsaMenuItemToolBar.CreateMemberObjects;
const OPNAME = 'TStomsaMenuItemToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FStomsaFileNewBtn        := CreateButton('StomsaFileNew');
    FStomsaFileOpenBtn       := CreateButton('StomsaFileOpen');
    FStomsaFileOpenParamBtn  := CreateButton('StomsaFileOpenParam');
    FStomsaFileSaveBtn       := CreateButton('StomsaFileSave');
    FStomsaFileMergeBtn      := CreateButton('StomsaFileMerge');
    FStomsaFileCloseBtn      := CreateButton('StomsaFileClose');
    FStomsaFileExportBtn     := CreateButton('StomsaFileExport');
    FStomsaFileImportBtn     := CreateButton('StomsaFileImport');

    FStomsaFileNewBtn.OnClick       := OnFileNewBtnClick;
    FStomsaFileOpenBtn.OnClick      := OnFileOpenBtnClick;
    FStomsaFileOpenParamBtn.OnClick := OnFileOpenParamBtnClick;
    FStomsaFileSaveBtn.OnClick      := OnFileSaveBtnClick;
    FStomsaFileMergeBtn.OnClick     := OnFileMergeBtnClick;
    FStomsaFileCloseBtn.OnClick     := OnFileCloseBtnClick;
    FStomsaFileExportBtn.OnClick    := OnFileExportBtnClick;
    FStomsaFileImportBtn.OnClick    := OnFileImportBtnClick;
    SetHorizontalPositions;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemToolBar.SetHorizontalPositions;
const OPNAME = 'TStomsaMenuItemToolBar.SetHorizontalPositions';
var LButtonCount, LGaps: integer;
begin
  try
    inherited SetHorizontalPositions;
    LButtonCount := -1;
    LGaps :=  0;
    SetButtonHorizontalPosition(FStomsaFileNewBtn, True,  True, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FStomsaFileOpenBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FStomsaFileOpenParamBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FStomsaFileSaveBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FStomsaFileMergeBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FStomsaFileCloseBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FStomsaFileExportBtn, True,  False, LButtonCount, LGaps);
    SetButtonHorizontalPosition(FStomsaFileImportBtn, True,  False, LButtonCount, LGaps);
    Width := FStomsaFileImportBtn.Left + FStomsaFileImportBtn.Width;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStomsaMenuItemToolBar.LanguageHasChanged: boolean;
const OPNAME = 'TStomsaMenuItemToolBar.LanguageHasChanged';
begin
  Result := True;
  try
    inherited LanguageHasChanged;
    FStomsaFileNewBtn.LanguageHasChanged;
    FStomsaFileOpenBtn.LanguageHasChanged;
    FStomsaFileOpenParamBtn.LanguageHasChanged;
    FStomsaFileSaveBtn.LanguageHasChanged;
    FStomsaFileMergeBtn.LanguageHasChanged;
    FStomsaFileCloseBtn.LanguageHasChanged;
    FStomsaFileExportBtn.LanguageHasChanged;
    FStomsaFileImportBtn.LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemToolBar.OnFileNewBtnClick(Sender: TObject);
const OPNAME = 'TStomsaMenuItemToolBar.OnFileNewBtnClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFileNew, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaMenuItemToolBar.OnFileOpenBtnClick(Sender: TObject);
const OPNAME = 'TStomsaMenuItemToolBar.OnFileOpenBtnClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFileOpen, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaMenuItemToolBar.OnFileOpenParamBtnClick(Sender: TObject);
const OPNAME = 'TStomsaMenuItemToolBar.OnFileOpenParamBtnClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFileOpenParam, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaMenuItemToolBar.OnFileSaveBtnClick(Sender: TObject);
const OPNAME = 'TStomsaMenuItemToolBar.OnFileSaveBtnClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFileSave, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaMenuItemToolBar.OnFileMergeBtnClick(Sender: TObject);
const OPNAME = 'TStomsaMenuItemToolBar.OnFileMergeBtnClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFileMerge, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaMenuItemToolBar.OnFileCloseBtnClick(Sender: TObject);
const OPNAME = 'TStomsaMenuItemToolBar.OnFileCloseBtnClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFileClose, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaMenuItemToolBar.OnFileExportBtnClick(Sender: TObject);
const OPNAME = 'TStomsaMenuItemToolBar.OnFileExportBtnClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFileExport, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaMenuItemToolBar.OnFileImportBtnClick(Sender: TObject);
const OPNAME = 'TStomsaMenuItemToolBar.OnFileImportBtnClick';
begin
  try
    FAppModules.Model.ProcessEvent(CmeStomsaFileImport, nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TStomsaMenuItemToolBar.SetStomsaFileNew(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemToolBar.SetStomsaFileNew';
begin
  try
    SetButtonEnabled(FStomsaFileNewBtn, AEnabled, 'StomsaFileNewDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemToolBar.SetStomsaFileOpen(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemToolBar.SetStomsaFileOpen';
begin
  try
    SetButtonEnabled(FStomsaFileOpenBtn, AEnabled, 'StomsaFileOpenDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemToolBar.SetStomsaFileSave(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemToolBar.SetStomsaFileSave';
begin
  try
    SetButtonEnabled(FStomsaFileSaveBtn, AEnabled, 'StomsaFileSaveDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemToolBar.SetStomsaFileMerge(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemToolBar.SetStomsaFileMerge';
begin
  try
    SetButtonEnabled(FStomsaFileMergeBtn, AEnabled, 'StomsaFileMergeDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemToolBar.SetStomsaFileClose(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemToolBar.SetStomsaFileClose';
begin
  try
    SetButtonEnabled(FStomsaFileCloseBtn, AEnabled, 'StomsaFileCloseDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemToolBar.SetStomsaFileOpenParam(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemToolBar.SetStomsaFileOpenParam';
begin
  try
    SetButtonEnabled(FStomsaFileOpenParamBtn, AEnabled, 'StomsaFileOpenParamDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemToolBar.SetStomsaFileExport(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemToolBar.SetStomsaFileExport';
begin
  try
    SetButtonEnabled(FStomsaFileExportBtn, AEnabled, 'StomsaFileExportDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStomsaMenuItemToolBar.SetStomsaFileImport(AEnabled: boolean);
const OPNAME = 'TStomsaMenuItemToolBar.SetStomsaFileImport';
begin
  try
    SetButtonEnabled(FStomsaFileImportBtn, AEnabled, 'StomsaFileImportDisabled');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
