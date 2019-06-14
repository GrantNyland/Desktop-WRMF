//
//  UNIT      : Contains TDbTableDataManager Class
//  AUTHOR    : Philemon Setshedi(PDNA)
//  DATE      : 2004/04/08
//  COPYRIGHT : Copyright © 2004 DWAF
//

unit UStudyCopyForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,

  UAbstractComponent,
  UAbstractModelObjects,
  UHelpContexts,
  UStudyObjects,
  UAbstractObject;

type
  TfrmStudyCopyForm = class(TAbstractForm)
    dlgWYRMFileSelector: TOpenDialog;
    pnlButtons: TPanel;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    pnlStudy: TPanel;
    lblStudyCaption: TLabel;
    lblStudyLabelCaption: TLabel;
    lblStudyClientCaption: TLabel;
    lblStudyConsultantCaption: TLabel;
    lblStudyNumberCaption: TLabel;
    lblStudyDateCaption: TLabel;
    lblStudyDescrCaption: TLabel;
    edtStudy: TEdit;
    edtStudyLabel: TEdit;
    edtStudyClient: TEdit;
    edtStudyConsultant: TEdit;
    edtStudyNumber: TEdit;
    dtpStudyDate: TDateTimePicker;
    memoStudyDescr: TMemo;
    pnlSubArea: TPanel;
    lblSubAreaCaption: TLabel;
    lblSubAreaLabelCaption: TLabel;
    lblSubAreDescr: TLabel;
    edtSubArea: TEdit;
    edtSubAreaLabel: TEdit;
    memoSubAreaDescr: TMemo;
    pnlScenario: TPanel;
    lblScenarioCaption: TLabel;
    lblScenarioLabelCaption: TLabel;
    lblScenarioDescr: TLabel;
    lblVersion: TLabel;
    edtScenario: TEdit;
    edtScenarioLabel: TEdit;
    memoScenarioDescr: TMemo;
    edtVersion: TEdit;
    lblMaxChars: TLabel;
    procedure edtStudyExit(Sender: TObject);
    procedure edtSubAreaExit(Sender: TObject);
    procedure edtScenarioExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
  protected
    FSelectionLevel: TModelActionLevel;
    FModelHasChanged,
    FStudyHasChanged,
    FSubAreHasChanged,
    FScenarioHasChanged: boolean;
    FStudyFields: TStudyFields;

    procedure DisableAllControls;
    procedure EnableAllControls;

    procedure SetStudyState(AEnabled: boolean);
    procedure SetSubAreaState(AEnabled: boolean);
    procedure SetScenarioState(AEnabled: boolean);

    procedure SetFormInitialState;
    procedure SetVisibleState;
    procedure ClearSubAreaData;
    procedure ClearScenarioData;

    function  ValidateIndexFields: boolean;
    procedure AssignHelpContext; override;

  public
    function LanguageHasChanged: boolean; override;
    function PopulateEditDialog(AStudyFields: TStudyFields): boolean;
    function ValidateEditDialog(var AStudyFields: TStudyFields): boolean;
    property StudyNameHasChanged : boolean read FStudyHasChanged write FStudyHasChanged;
    property SubAreHasChanged : boolean read FSubAreHasChanged write FSubAreHasChanged;
    property ScenarioHasChanged : boolean read FScenarioHasChanged write FScenarioHasChanged;
    property SelectionLevel : TModelActionLevel read FSelectionLevel write FSelectionLevel;
  end;

implementation

uses
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

{$R *.dfm}

const
  C_IdHeight = 'StudyFormHeight';
  C_DefaultHeight   = 480;

procedure TfrmStudyCopyForm.AssignHelpContext;
const OPNAME = 'TfrmStudyCopyForm.AssignHelpContext';
begin
  try
    SetAllControlsHelpContext(Self,HC_StudyEdit);
    SetAllControlsHelpContext(pnlStudy,HC_StudyEditStudyPanel);
    SetAllControlsHelpContext(pnlSubArea,HC_StudyEditSubAreaPanel);
    SetAllControlsHelpContext(pnlScenario,HC_StudyEditScenarioPanel);

    SetControlHelpContext(edtStudy,HC_StudyEditStudy);
    SetControlHelpContext(edtStudyLabel,HC_StudyEditStudyName);
    SetControlHelpContext(edtStudyClient,HC_StudyEditClient);
    SetControlHelpContext(edtStudyConsultant,HC_StudyEditConsultant);
    SetControlHelpContext(edtStudyNumber,HC_StudyEditStudyNumber);
    SetControlHelpContext(dtpStudyDate,HC_StudyEditStudyDate);
    SetControlHelpContext(memoStudyDescr,HC_StudyEditStudyDescr);

    SetControlHelpContext(edtSubArea,HC_StudyEditSubArea);
    SetControlHelpContext(edtSubAreaLabel,HC_StudyEditSubAreaName);
    SetControlHelpContext(memoSubAreaDescr,HC_StudyEditSubAreaDescr);

    SetControlHelpContext(edtScenario,HC_StudyEditScenario);
    SetControlHelpContext(edtScenarioLabel,HC_StudyEditScenarioName);
    SetControlHelpContext(memoScenarioDescr,HC_StudyEditScenarioDescr);
    //SetControlHelpContext(edtScenarioFilesPrefix,HC_StudyEditFilesPrefix);
    //SetControlHelpContext(btnScenarioPath,HC_StudyEditDirectorySelector);

    SetControlHelpContext(btnCancel,HC_StudyEditCancelButton);
    SetControlHelpContext(btnOk,HC_StudyEditSaveButton);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TfrmStudyCopyForm.LanguageHasChanged: boolean;
const OPNAME = 'TfrmStudyCopyForm.LanguageHasChanged';
begin
  Result := False;
  try
    lblStudyCaption.Caption := FAppModules.Language.GetString(ClassName + '.StudyLabel') + ' :';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.SetStudyState(AEnabled: boolean);
const OPNAME = 'TfrmStudyCopyForm.SetStudyState';
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

    lblStudyLabelCaption.Enabled        := AEnabled;
    edtStudyLabel.Enabled               := AEnabled;
    edtStudyLabel.Color                 := LColor;

    lblStudyClientCaption.Enabled       := AEnabled;
    edtStudyClient.Enabled              := AEnabled;
    edtStudyClient.Color                := LColor;

    lblStudyConsultantCaption.Enabled   := AEnabled;
    edtStudyConsultant.Enabled          := AEnabled;
    edtStudyConsultant.Color            := LColor;

    lblStudyNumberCaption.Enabled       := AEnabled;
    edtStudyNumber.Enabled              := AEnabled;
    edtStudyNumber.Color                := LColor;

    lblStudyDateCaption.Enabled         := AEnabled;
    dtpStudyDate.Enabled                := AEnabled;
    dtpStudyDate.Color                  := LColor;

    lblStudyDescrCaption.Enabled        := AEnabled;
    memoStudyDescr.Enabled              := AEnabled;
    memoStudyDescr.Color                := LColor;

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.SetSubAreaState(AEnabled: boolean);
const OPNAME = 'TfrmStudyCopyForm.SetSubAreaState';
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

    lblSubAreaLabelCaption.Enabled      := AEnabled;
    edtSubAreaLabel.Enabled             := AEnabled;
    edtSubAreaLabel.Color               := LColor;

    lblSubAreDescr.Enabled              := AEnabled;
    memoSubAreaDescr.Enabled            := AEnabled;
    memoSubAreaDescr.Color              := LColor;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.SetScenarioState(AEnabled: boolean);
const OPNAME = 'TfrmStudyCopyForm.SetScenarioState';
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

    lblScenarioLabelCaption.Enabled     := AEnabled;
    edtScenarioLabel.Enabled            := AEnabled;
    edtScenarioLabel.Color              := LColor;

    memoScenarioDescr.Enabled           := AEnabled;
    memoScenarioDescr.Color             := LColor;

    lblVersion.Enabled                  := AEnabled;
    edtVersion.Enabled                  := AEnabled;
    edtVersion.Color                    := LColor;

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.ClearSubAreaData;
const OPNAME = 'TfrmStudyCopyForm.ClearSubAreaData';
begin
  try
    edtSubArea.Text := '';
    edtSubAreaLabel.Text := '';
    memoSubAreaDescr.Lines.Text := '';
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.ClearScenarioData;
const OPNAME = 'TfrmStudyCopyForm.ClearScenarioData';
begin
  try
    edtScenario.Text := '';
    edtScenarioLabel.Text := '';
    memoScenarioDescr.Lines.Text := '';
    edtVersion.Text := '6';

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.DisableAllControls;
const OPNAME = 'TfrmStudyCopyForm.DisableAllControls';
begin
  try
    btnOk.Enabled := False;
    SetStudyState(False);
    SetSubAreaState(False);
    SetScenarioState(False);
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.EnableAllControls;
const OPNAME = 'TfrmStudyCopyForm.EnableAllControls';
begin
  try
    SetStudyState(True);
    SetSubAreaState(True);
    SetScenarioState(True);
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmStudyCopyForm.PopulateEditDialog(AStudyFields: TStudyFields): boolean;
const OPNAME = 'TfrmStudyCopyForm.PopulateEditDialog';
begin

  Result := False;
  try
    LanguageHasChanged;
    if not Assigned(AStudyFields) then
       raise Exception.Create('Study fields parameter is not yet assigned.');

    FStudyFields := AStudyFields;
    edtStudy.Text        := AStudyFields.FStudyAreaName;
    edtSubArea.Text      := AStudyFields.FSubArea;
    edtScenario.Text     := AStudyFields.FScenario;

    dtpStudyDate.DateTime         :=  AStudyFields.FStudyDate;
    edtStudyConsultant.Text       :=  AStudyFields.FConsultant;
    edtStudyClient.Text           :=  AStudyFields.FClient;
    edtStudyNumber.Text           :=  AStudyFields.FStudyNumber;
    edtStudyLabel.Text            :=  AStudyFields.FStudyLabel;
    memoStudyDescr.Lines.Text     :=  AStudyFields.FStudyAreaDescr;

    edtSubAreaLabel.Text        := AStudyFields.FSubAreaLabel;
    memoSubAreaDescr.Lines.Text := AStudyFields.FSubAreaDescr;


    edtScenarioLabel.Text        := AStudyFields.FScenarioLabel;
    memoScenarioDescr.Lines.Text := AStudyFields.FScenarioDescr;
    edtVersion.Text            :=  AStudyFields.FVersion;
    if(AStudyFields.FModel = CHydrology) then
    begin
      edtVersion.Text := '1';
      edtVersion.Visible := False;
      lblVersion.Visible := False;
      lblMaxChars.Visible := True;
      lblScenarioCaption.Caption       := 'Network Code';
      lblScenarioLabelCaption.Caption  := 'Code Descr';
    end;

    SetFormInitialState;
    Result := True;

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.SetFormInitialState;
const OPNAME = 'TfrmStudyCopyForm.SetFormInitialState';
begin
  try
    DisableAllControls;
    SetVisibleState;
    FModelHasChanged := False;
    FAppModules.ViewIni.ReadInteger(ClassName,C_IdHeight,C_DefaultHeight);
    //Height := 250;
    case FSelectionLevel of
      malStudy:
        begin
          pnlStudy.Align := alClient;
          SetStudyState(True);
        end;
      malSubArea:
        begin
          SetSubAreaState(True);
          pnlSubArea.Align := alClient;
        end;
      malScenarion:
        begin
          pnlScenario.Align := alClient;
          SetScenarioState(True);
        end;
    end;//case
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmStudyCopyForm.ValidateEditDialog(var AStudyFields: TStudyFields): boolean;
const OPNAME = 'TfrmStudyCopyForm.ValidateEditDialog';
begin
  Result := False;
  try
    if not Assigned(AStudyFields) then
       raise Exception.Create('Study fields parameter is not yet assigned.');

    //AStudyFields.Reset;

    if not ValidateIndexFields then
    Exit;

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
    AStudyFields.FVersion        := edtVersion.Text;

    Result := (FStudyHasChanged or
              FSubAreHasChanged or
              FScenarioHasChanged);

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.edtStudyExit(Sender: TObject);
const OPNAME = 'TfrmStudyCopyForm.edtStudyExit';
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
    end;
    btnOk.Enabled := FStudyHasChanged;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.edtSubAreaExit(Sender: TObject);
const OPNAME = 'TfrmStudyCopyForm.edtSubAreaExit';
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
    btnOk.Enabled := FSubAreHasChanged;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.edtScenarioExit(Sender: TObject);
const OPNAME = 'TfrmStudyCopyForm.edtScenarioExit';
begin
  try
    if Sender is TEdit then
    begin
      if TEdit(Sender).Modified then
      FScenarioHasChanged := True;
    end;
    btnOk.Enabled := FScenarioHasChanged;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmStudyCopyForm.ValidateIndexFields: boolean;
const OPNAME = 'TfrmStudyCopyForm.ValidateIndexFields';
begin
  Result := False;
  try
    case FSelectionLevel of
     malStudy:
       begin
         Result := (Trim(edtStudy.Text) <> '') and
                   (Trim(edtStudyLabel.Text) <> '');
       end;
     malSubArea:
       begin
         Result := (Trim(edtStudy.Text) <> '') and
                   (Trim(edtStudyLabel.Text) <> '') and
                   (Trim(edtSubArea.Text) <> '') and
                   (Trim(edtSubAreaLabel.Text) <> '');
       end;
     malScenarion:
       begin
         Result := (Trim(edtScenario.Text) <> '') and
                   (Trim(edtScenarioLabel.Text) <> '') and
                   (Trim(edtVersion.Text) <> '');
       end;
     end;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TfrmStudyCopyForm.FormShow(Sender: TObject);
const OPNAME = 'TfrmStudyCopyForm.FormShow';
begin
  try
    case FNodeLevel of
      nlStudy:
      begin
        pnlSubArea.Hide;
        pnlScenario.Hide;
      end;
      nlSubArea:
      begin
        pnlStudy.Hide;
        pnlScenario.Hide;
      end;
      nlScenario:
      begin
        pnlStudy.Hide;
        pnlSubArea.Hide;
      end;
    end;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
procedure TfrmStudyCopyForm.SetVisibleState;
const OPNAME = 'TfrmStudyCopyForm.SetVisibleState';
begin
  try
    case FSelectionLevel of
      malStudy:
      begin
        pnlStudy.Show;
        pnlSubArea.Hide;
        pnlScenario.Hide;
      end;
      malSubArea:
      begin
        pnlSubArea.Show;
        pnlStudy.Hide;
        pnlScenario.Hide;
      end;
      malScenarion:
      begin
        pnlScenario.Show;
        pnlStudy.Hide;
        pnlSubArea.Hide;
      end;
    end;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.FormClose(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'TfrmStudyCopyForm.FormClose';
begin
  try
    FAppModules.ViewIni.WriteInteger(ClassName,C_IdHeight, Height);
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmStudyCopyForm.FormKeyPress(Sender: TObject; var Key: Char);
const OPNAME = 'TfrmStudyCopyForm.FormKeyPress';
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

end.
