//
//
//  UNIT      : Contains the class TStudySelectionButtonPanel.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/17
//  COPYRIGHT : Copyright © 2002 DWAF                  
//
//
unit UStudySelectionButtonPanel;

interface

uses
  Vcl.Buttons,
  UAbstractObject,
  UAbstractComponent;

type
  TStudySelectionButtonPanel = class(TAbstractPanel)
  protected
    FOKButton: TAbstractSpeedButton;
    FCancelButton: TAbstractSpeedButton;
    FReportButton: TAbstractSpeedButton;
    FNewButton: TAbstractSpeedButton;  //TBitBtn;
    FCopyButton: TAbstractSpeedButton;
    FEditButton: TAbstractSpeedButton;
    FDeleteButton: TAbstractSpeedButton;
    FStudyDocumentDetail: TStudyDocumentDetail;
    procedure CreateMemberObjects; override;
    procedure CreateButton(var AButton: TAbstractSpeedButton; AButtonKey: string; AIndex: integer);
    procedure AssignHelpContext; override;
//    procedure DoKeyPress ( Sender : TObject; var Key : Char );
  public
    procedure LaunchReport(ASender: TObject);
    procedure SetButtonStates(ACanOK, AReadOnly, ACanAdd, ACanDelete, ACanCopy: boolean; ADocumentDetail: TStudyDocumentDetail);
    property OKButton: TAbstractSpeedButton read FOKButton;
    property CancelButton: TAbstractSpeedButton read FCancelButton;
    property NewButton: TAbstractSpeedButton read FNewButton;
    property CopyButton: TAbstractSpeedButton read FCopyButton;
    property EditButton: TAbstractSpeedButton read FEditButton;
    property DeleteButton: TAbstractSpeedButton read FDeleteButton;
    property ReportButton: TAbstractSpeedButton read FReportButton;
  end;

implementation

uses
  SysUtils,
  Vcl.Controls,
  UConstants,
  UHelpContexts,
  UMainMenuEventType,
  UErrorHandlingOperations;

const
  C_ButtonGap    = 4;
  C_ButtonHeight = 25;
  C_ButtonWidth  = 75;

procedure TStudySelectionButtonPanel.AssignHelpContext;
const OPNAME = 'TStudySelectionButtonPanel.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,HC_StudyAreaSelection);
    SetControlHelpContext(FOKButton,HC_StudySelectionOK);
    SetControlHelpContext(FCancelButton,HC_StudySelectionCancel);
    SetControlHelpContext(FReportButton,HC_StudySelectionReport);
    SetControlHelpContext(FNewButton,HC_StudySelectionNew);
    SetControlHelpContext(FCopyButton,HC_StudySelectionCopy);
    SetControlHelpContext(FEditButton,HC_StudySelectionEdit);
    SetControlHelpContext(FDeleteButton,HC_StudySelectionDelete);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudySelectionButtonPanel.CreateMemberObjects;
const OPNAME = 'TStudySelectionButtonPanel.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    ShowHint := True;
    Height := C_ButtonHeight + C_ButtonGap * 2;
    CreateButton(FOKButton,     FAppModules.Language.GetString('ButtonCaption.OK'),     0);
    CreateButton(FCancelButton, FAppModules.Language.GetString('ButtonCaption.Cancel'), 1);
    CreateButton(FReportButton, FAppModules.Language.GetString('ButtonCaption.Report'), 2);
    CreateButton(FNewButton,    FAppModules.Language.GetString('ButtonCaption.New'),    3);
    CreateButton(FCopyButton,   FAppModules.Language.GetString('ButtonCaption.Copy'),   4);
    CreateButton(FEditButton,   FAppModules.Language.GetString('ButtonCaption.Edit'),   5);
    CreateButton(FDeleteButton, FAppModules.Language.GetString('ButtonCaption.Delete'), 6);
//    FOKButton.Kind := bkOK;
//    FCancelButton.Kind := bkCancel;
    FReportButton.OnClick := LaunchReport;
    FStudyDocumentDetail := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudySelectionButtonPanel.CreateButton(var AButton: TAbstractSpeedButton; AButtonKey: string; AIndex: integer);
const OPNAME = 'TStudySelectionButtonPanel.CreateButton';
begin
  try
    AButton := TAbstractSpeedButton.Create(self, FAppModules, AButtonKey );
    AButton.Parent  := self;
    AButton.Top     := C_ButtonGap;
    AButton.Left    := C_ButtonGap * 2 + (C_ButtonWidth + C_ButtonGap) * AIndex;
    AButton.Height  := C_ButtonHeight;
    AButton.Width   := C_ButtonWidth;
    AButton.Name    := AButtonKey;
    AButton.Caption := FAppModules.Language.GetString('ButtonCaption.' + AButtonKey);
    AButton.Hint    := FAppModules.Language.GetString('ButtonHint.' + AButtonKey);
    AButton.Glyph.LoadFromResourceName(HImagesInstance, UpperCase(AButtonKey));
    AButton.NumGlyphs := AButton.Glyph.Width div AButton.Glyph.Height;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudySelectionButtonPanel.SetButtonStates(ACanOK, AReadOnly, ACanAdd, ACanDelete, ACanCopy: boolean;
                                                    ADocumentDetail: TStudyDocumentDetail);
const OPNAME = 'TStudySelectionButtonPanel.SetButtonStates';
begin
  try
    // Can the user select the current study.
    FOKButton.Enabled := ACanOK;
    if not ACanOK then
    begin
      FOKButton.StatusReason := FOKButton.ButtonKey;
      FOKButton.LanguageHasChanged;
    end
    else
    begin
      FOKButton.StatusReason := '';
      FOKButton.LanguageHasChanged;
    end;
    // Is a reoprt set for this level.
    if Assigned(ADocumentDetail) then
    begin
      FStudyDocumentDetail := ADocumentDetail;
      FReportButton.Enabled := True;
      FReportButton.StatusReason := '';
      FReportButton.LanguageHasChanged;
    end else begin
      FStudyDocumentDetail := nil;
      FReportButton.Enabled := False;
      FReportButton.StatusReason := FReportButton.ButtonKey;
      FReportButton.LanguageHasChanged;
    end;
    FEditButton.Enabled   := AReadOnly;
    if not AReadOnly then
    begin
      FEditButton.StatusReason := FEditButton.ButtonKey;
      FEditButton.LanguageHasChanged;
    end
    else
    begin
      FEditButton.StatusReason := '';
      FEditButton.LanguageHasChanged;
    end;

    FDeleteButton.Enabled := ACanDelete;
    if not ACanDelete then
    begin
      FDeleteButton.StatusReason := FDeleteButton.ButtonKey;
      FDeleteButton.LanguageHasChanged;
    end
    else
    begin
      FDeleteButton.StatusReason := '';
      FDeleteButton.LanguageHasChanged;
    end;
    FNewButton.Enabled    := ACanAdd;
    if not ACanAdd then
    begin
      FNewButton.StatusReason := FNewButton.ButtonKey;
      FNewButton.LanguageHasChanged;
    end
    else
    begin
      FNewButton.StatusReason := '';
      FNewButton.LanguageHasChanged;
    end;
    FCopyButton.Enabled   := ACanCopy;
    if not ACanCopy then
    begin
      FCopyButton.StatusReason := FCopyButton.ButtonKey;
      FCopyButton.LanguageHasChanged;
    end
    else
    begin
      FCopyButton.StatusReason := '';
      FCopyButton.LanguageHasChanged;
    end;
  // Handle all exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TStudySelectionButtonPanel.LaunchReport(ASender: TObject);
const OPNAME = 'TStudySelectionButtonPanel.LaunchReport';
begin
  try
    if Assigned(FStudyDocumentDetail) then
      FAppModules.ProcessEvent(CmeLaunchStudyReport, FStudyDocumentDetail);
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
