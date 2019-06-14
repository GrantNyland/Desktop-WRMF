{******************************************************************************}
{*  UNIT      : Contains the class TChangeAdminToolbar.                       *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/02/11                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChangeAdminToolBar;

interface

uses
  Vcl.stdctrls,
  Vcl.controls,
  UAbstractComponent,
  UChildToolBar;

type

  TChangeAdminToolBar = class(TChildToolBar)
  protected
    FCreateChangeGroupBtn     : TAbstractSpeedButton;
    FDeleteChangeGroupBtn     : TAbstractSpeedButton;

    FMoveUpChangeElementBtn     : TAbstractSpeedButton;
    FMoveDownChangeElementBtn   : TAbstractSpeedButton;
    FActivateChangeElementBtn   : TAbstractSpeedButton;
    FDeactivateChangeElementBtn : TAbstractSpeedButton;

    FCreateChangeListBtn      : TAbstractSpeedButton;
    FDeleteChangeListBtn      : TAbstractSpeedButton;
    FCopyChangeListBtn        : TAbstractSpeedButton;
    FApplyChangeListBtn       : TAbstractSpeedButton;
    FExportChangeListBtn      : TAbstractSpeedButton;
    FImportChangeListBtn      : TAbstractSpeedButton;
    FFilterStationBtn         : TAbstractSpeedButton;
    procedure CreateMemberObjects; override;
    procedure SetHorizontalPositions; override;
    procedure OnClickCreateNewChangeGroup(ASender: TObject);
    procedure OnClickDeleteChangeGroup(ASender: TObject);
//    procedure OnClickActivateChangeGroup(ASender: TObject);
//    procedure OnClickDeactivateChangeGroup(ASender: TObject);
    procedure OnClickCreateNewChangeList(ASender: TObject);
    procedure OnClickDeleteChangeList(ASender: TObject);
    procedure OnClickCopyChangeList(ASender: TObject);
    procedure OnClickMoveUpChangeElement(ASender: TObject);
    procedure OnClickMoveDownChangeElement(ASender: TObject);
    procedure OnClickActivateChangeElement(ASender: TObject);
    procedure OnClickDeactivateChangeElement(ASender: TObject);
    procedure OnClickApplyChangeList(ASender: TObject);
    procedure OnClickImportChangeList(ASender: TObject);
    procedure OnClickExportChangeList(ASender: TObject);
    procedure OnClickFilterStation(ASender: TObject);
  public
    function Initialise : Boolean; override;
    function LanguageHasChanged : boolean; override;
    procedure SetCreateNewChangeGroup (AEnabled : boolean);
    procedure SetDeleteChangeGroup (AEnabled : boolean);
    procedure SetCreateNewChangeList (AEnabled : boolean);
    procedure SetDeleteChangeList (AEnabled : boolean);
    procedure SetCopyChangeList (AEnabled : boolean);
    procedure SetMoveUpChangeElement (AEnabled : boolean);
    procedure SetMoveDownChangeElement (AEnabled : boolean);
    procedure SetActivateChangeElement (AEnabled : boolean);
    procedure SetDeactivateChangeElement (AEnabled : boolean);
    procedure SetApplyChangeList (AEnabled : boolean);
    procedure SetExportChangeList (AEnabled : boolean);
    procedure SetImportChangeList (AEnabled : boolean);
    procedure SetFilterStation (AEnabled : boolean);

    property CreateChangeGroupBtn       : TAbstractSpeedButton read FCreateChangeGroupBtn;
    property DeleteChangeGroupBtn       : TAbstractSpeedButton read FDeleteChangeGroupBtn;
    property CreateChangeListBtn        : TAbstractSpeedButton read FCreateChangeListBtn;
    property DeleteChangeListBtn        : TAbstractSpeedButton read FDeleteChangeListBtn;
    property CopyChangeListBtn          : TAbstractSpeedButton read FCopyChangeListBtn;
    property MoveUpChangeElementBtn     : TAbstractSpeedButton read FMoveUpChangeElementBtn;
    property MoveDownChangeElementBtn   : TAbstractSpeedButton read FMoveDownChangeElementBtn;
    property ActivateChangeElementBtn   : TAbstractSpeedButton read FActivateChangeElementBtn;
    property DeactivateChangeElementBtn : TAbstractSpeedButton read FDeactivateChangeElementBtn;
    property ApplyChangeListBtn         : TAbstractSpeedButton read FApplyChangeListBtn;
    property ImportChangeListBtn        : TAbstractSpeedButton read FImportChangeListBtn;
    property ExportChangeListBtn        : TAbstractSpeedButton read FExportChangeListBtn;
    property FilterStationBtn           : TAbstractSpeedButton read FFilterStationBtn;
end;

implementation

uses
  windows,
  SysUtils,
  UGenericModelLinkClasses,
  UMainMenuEventType,
  UErrorHandlingOperations;

{ TChangeAdminToolBar }

procedure TChangeAdminToolBar.CreateMemberObjects;
const OPNAME = 'TChangeAdminToolBar.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FCreateChangeGroupBtn     := CreateButton('CGCreateNew');
    FDeleteChangeGroupBtn     := CreateButton('CGDelete');

    FActivateChangeElementBtn   := CreateButton('CEActivate');
    FDeactivateChangeElementBtn := CreateButton('CEDeactivate');
    FMoveUpChangeElementBtn     := CreateButton('CLMoveUp');
    FMoveDownChangeElementBtn   := CreateButton('CLMoveDown');

    FCreateChangeListBtn        := CreateButton('CLCreateNew');
    FDeleteChangeListBtn        := CreateButton('CLDelete');
    FCopyChangeListBtn          := CreateButton('CLCopy');
    FApplyChangeListBtn         := CreateButton('CLApply');
    FImportChangeListBtn         := CreateButton('CLImport');
    FExportChangeListBtn         := CreateButton('CLExport');
    FFilterStationBtn            := CreateButton('CStationFilter');

    SetHorizontalPositions;
    FCreateChangeGroupBtn.OnClick       := OnClickCreateNewChangeGroup;
    FDeleteChangeGroupBtn.OnClick       := OnClickDeleteChangeGroup;
    FCreateChangeListBtn.OnClick        := OnClickCreateNewChangeList;
    FDeleteChangeListBtn.OnClick        := OnClickDeleteChangeList;
    FCopyChangeListBtn.OnClick          := OnClickCopyChangeList;
    FMoveUpChangeElementBtn.OnClick     := OnClickMoveUpChangeElement;
    FMoveDownChangeElementBtn.OnClick   := OnClickMoveDownChangeElement;
    FActivateChangeElementBtn.OnClick   := OnClickActivateChangeElement;
    FDeactivateChangeElementBtn.OnClick := OnClickDeactivateChangeElement;
    FApplyChangeListBtn.OnClick         := OnClickApplyChangeList;
    FImportChangeListBtn.OnClick        := OnClickImportChangeList;
    FExportChangeListBtn.OnClick        := OnClickExportChangeList;
    FFilterStationBtn.OnClick           := OnClickFilterStation;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeAdminToolBar.Initialise : Boolean;
const OPNAME = 'TChangeAdminToolBar.Initialise';
begin
  Result := True;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeAdminToolBar.LanguageHasChanged : boolean;
const OPNAME = 'TChangeAdminToolBar.LanguageHasChanged';
begin
  Result := True;
  inherited LanguageHasChanged;
  try
    FCreateChangeGroupBtn.LanguageHasChanged;
    FDeleteChangeGroupBtn.LanguageHasChanged;
    FCreateChangeListBtn.LanguageHasChanged;
    FDeleteChangeListBtn.LanguageHasChanged;
    FCopyChangeListBtn.LanguageHasChanged;
    FMoveUpChangeElementBtn.LanguageHasChanged;
    FMoveDownChangeElementBtn.LanguageHasChanged;
    FActivateChangeElementBtn.LanguageHasChanged;
    FDeactivateChangeElementBtn.LanguageHasChanged;
    FApplyChangeListBtn.LanguageHasChanged;
    FImportChangeListBtn.LanguageHasChanged;
    FExportChangeListBtn.LanguageHasChanged;
    FFilterStationBtn.LanguageHasChanged;

    FCreateChangeGroupBtn.Hint       := FAppModules.Language.GetString('ButtonHint.CGCreateNew');
    FDeleteChangeGroupBtn.Hint       := FAppModules.Language.GetString('ButtonHint.CGDelete');
    FCreateChangeListBtn.Hint        := FAppModules.Language.GetString('ButtonHint.CLCreateNew');
    FDeleteChangeListBtn.Hint        := FAppModules.Language.GetString('ButtonHint.CLDelete');
    FCopyChangeListBtn.Hint          := FAppModules.Language.GetString('ButtonHint.CLCopy');
    FMoveUpChangeElementBtn.Hint     := FAppModules.Language.GetString('ButtonHint.CLMoveUp');
    FMoveDownChangeElementBtn.Hint   := FAppModules.Language.GetString('ButtonHint.CLMoveDown');
    FActivateChangeElementBtn.Hint   := FAppModules.Language.GetString('ButtonHint.CEActivate');
    FDeactivateChangeElementBtn.Hint := FAppModules.Language.GetString('ButtonHint.CEDeactivate');
    FApplyChangeListBtn.Hint         := FAppModules.Language.GetString('ButtonHint.CLApply');
    FImportChangeListBtn.Hint        := FAppModules.Language.GetString('ButtonHint.CLImport');
    FExportChangeListBtn.Hint        := FAppModules.Language.GetString('ButtonHint.CLExport');
    FFilterStationBtn.Hint           := FAppModules.Language.GetString('ButtonHint.CStationFilter');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetCreateNewChangeGroup (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetCreateNewChangeGroup';
begin
  try
    SetButtonEnabled(FCreateChangeGroupBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetDeleteChangeGroup (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetDeleteChangeGroup';
begin
  try
    SetButtonEnabled(FDeleteChangeGroupBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetCreateNewChangeList (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetCreateNewChangeList';
begin
  try
    SetButtonEnabled(FCreateChangeListBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetDeleteChangeList (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetDeleteChangeList';
begin
  try
    SetButtonEnabled(FDeleteChangeListBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetCopyChangeList (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetCopyChangeList';
begin
  try
    SetButtonEnabled(FCopyChangeListBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetMoveUpChangeElement (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetMoveUpChangeElement';
begin
  try
    SetButtonEnabled(FMoveUpChangeElementBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetMoveDownChangeElement (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetMoveDownChangeElement';
begin
  try
    SetButtonEnabled(FMoveDownChangeElementBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetActivateChangeElement (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetActivateChangeElement';
begin
  try
    SetButtonEnabled(FActivateChangeElementBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetDeactivateChangeElement (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetDeactivateChangeElement';
begin
  try
    SetButtonEnabled(FDeactivateChangeElementBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetApplyChangeList (AEnabled : boolean);
const OPNAME = 'TChangeAdminToolBar.SetApplyChangeList';
begin
  try
    SetButtonEnabled(FApplyChangeListBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetHorizontalPositions;
const OPNAME = 'TChangeAdminToolBar.SetHorizontalPositions';
var
   lButtonCount,
   lGaps        : integer;
begin
  inherited SetHorizontalPositions;
  lButtonCount := -1;
  lGaps := 0;
  SetButtonHorizontalPosition(FCreateChangeGroupBtn,       True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FDeleteChangeGroupBtn,       True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FCreateChangeListBtn,        True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FDeleteChangeListBtn,        True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FCopyChangeListBtn,          True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FMoveUpChangeElementBtn,     True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FMoveDownChangeElementBtn,   True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FActivateChangeElementBtn,   True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FDeactivateChangeElementBtn, True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FApplyChangeListBtn,         True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FImportChangeListBtn,        True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FExportChangeListBtn,        True, False, lButtonCount, lGaps);
  SetButtonHorizontalPosition(FFilterStationBtn,        True, False, lButtonCount, lGaps);
  Width := FFilterStationBtn.Left + FFilterStationBtn.Width;
end;

procedure TChangeAdminToolBar.OnClickCreateNewChangeGroup(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickCreateNewChangeGroup';
begin
  try
    FAppModules.Changes.DoCreateNewChangeGroup;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickDeleteChangeGroup(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickDeleteChangeGroup';
begin
  try
    FAppModules.Changes.DoDeleteChangeGroup(-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
(*
procedure TChangeAdminToolBar.OnClickActivateChangeGroup(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickActivateChangeGroup';
begin
  try
    FAppModules.Changes.DoActivateChangeElement(-1, 0, FALSE);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickDeactivateChangeGroup(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickDeactivateChangeGroup';
begin
  try
    FAppModules.Changes.DoDeactivateChangeElement(-1,0,FALSE);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
*)
procedure TChangeAdminToolBar.OnClickCreateNewChangeList(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickCreateNewChangeList';
begin
  try
    FAppModules.Changes.DoCreateNewChangeList;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickDeleteChangeList(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickDeleteChangeList';
begin
  try
    FAppModules.Changes.DoDeleteChangeList(-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickCopyChangeList(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickCopyChangeList';
begin
  try
    FAppModules.Changes.DoCopyChangeList(-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickMoveUpChangeElement(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickMoveUpChangeElement';
begin
  try
    FAppModules.Changes.DoMoveUpChangeElement(0,0,TRUE);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickMoveDownChangeElement(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickMoveDownChangeElement';
begin
  try
    FAppModules.Changes.DoMoveDownChangeElement(0,0,TRUE);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickActivateChangeElement(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickActivateChangeElement';
begin
  try
    FAppModules.Changes.DoActivateChangeElement(0,0,TRUE);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickDeactivateChangeElement(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickDeactivateChangeElement';
begin
  try
    FAppModules.Changes.DoDeactivateChangeElement(0,0,TRUE);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickApplyChangeList(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickApplyChangeList';
begin
  try
    FAppModules.Changes.DoApplyChangeList(-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickExportChangeList(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickExportChangeList';
begin
  try
    FAppModules.Changes.DoExportChangeList(-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickImportChangeList(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickImportChangeList';
begin
  try
    FAppModules.Changes.DoImportChangeList(-1);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetExportChangeList(AEnabled: boolean);
const OPNAME = 'TChangeAdminToolBar.SetExportChangeList';
begin
  try
    SetButtonEnabled(FExportChangeListBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetImportChangeList(AEnabled: boolean);
const OPNAME = 'TChangeAdminToolBar.SetImportChangeList';
begin
  try
    SetButtonEnabled(FImportChangeListBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.OnClickFilterStation(ASender: TObject);
const OPNAME = 'TChangeAdminToolBar.OnClickFilterStation';
begin
  try
  // FAppModules.Changes.DoStationFilter(-1);
    FAppModules.Model.ProcessEvent(CmeChangeListStationFilter,nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TChangeAdminToolBar.SetFilterStation(AEnabled: boolean);
const OPNAME = 'TChangeAdminToolBar.SetFilterStation';
begin
  try
    SetButtonEnabled(FFilterStationBtn, AEnabled, '');
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
