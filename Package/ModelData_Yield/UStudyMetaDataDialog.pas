unit UStudyMetaDataDialog;

interface
uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.DBCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type
  TStudyMetaDataDialog = class(TAbstractScrollablePanel)
  protected
    FImportedByLabel       : TLabel;
    FImportedByEdit        : TFieldEdit;
    FErrorTypeLabel        : TLabel;
    FErrorTypeEdit         : TFieldRichEdit;
    FErrorDescriptionLabel : TLabel;
    FErrorDescriptionEdit  : TFieldRichEdit;
    FStudyErrorsLabel      : TLabel;
    FStudyErrorsComboBox   : TFieldComboBox;
    FCorrectiveActionLabel : TLabel;
    FCorrectiveActionEdit  : TFieldRichEdit;
    FCreateBtn             : TFieldBitBtn;
    FDeleteBtn             : TFieldBitBtn;
    FPriorBtn              : TFieldBitBtn;
    FFirstBtn              : TFieldBitBtn;
    FLastBtn               : TFieldBitBtn;
    FNextBtn               : TFieldBitBtn;

    procedure CreateMemberObjects; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;

    property ImportedByEdit       : TFieldEdit     read FImportedByEdit;
    property ErrorTypeEdit        : TFieldRichEdit read FErrorTypeEdit;
    property ErrorDescriptionEdit : TFieldRichEdit read FErrorDescriptionEdit;
    property StudyErrorsComboBox  : TFieldComboBox read FStudyErrorsComboBox;
    property CorrectiveActionEdit : TFieldRichEdit read FCorrectiveActionEdit;
    property CreateBtn            : TFieldBitBtn   read FCreateBtn;
    property DeleteBtn            : TFieldBitBtn   read FDeleteBtn;
    property PriorBtn             : TFieldBitBtn   read FPriorBtn;
    property FirstBtn             : TFieldBitBtn   read FFirstBtn;
    property LastBtn              : TFieldBitBtn   read FLastBtn;
    property NextBtn              : TFieldBitBtn   read FNextBtn;
  end;

implementation
uses
  SysUtils,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{ TStudyMetaDataDialog }

procedure TStudyMetaDataDialog.CreateMemberObjects;
const OPNAME = 'TStudyMetaDataDialog.CreateMemberObjects';
var
  LOwner  : TComponent;
  LParent : TWinControl;
begin
  inherited;
  try
    LOwner  := ControlsOwner;
    LParent := ControlsParent;

    FCreateBtn := TFieldBitBtn.Create(LOwner,FAppModules);
    with FCreateBtn do
    begin
      Parent    := LParent;
      Left      := 140;
      Top       := 10;
      Width     := 50;
      Height    := 25;
      TabStop   := TRUE;
      TabOrder  := 1;
      Glyph.LoadFromResourceName(HImagesInstance,'CREATEIFRSITE');
    end;

    FDeleteBtn := TFieldBitBtn.Create(LOwner,FAppModules);
    with FDeleteBtn do
    begin
      Parent    := LParent;
      Left      := 200;
      Top       := 10;
      Width     := 50;
      Height    := 25;
      TabStop   := TRUE;
      TabOrder  := 2;
      Glyph.LoadFromResourceName(HImagesInstance,'DELETEIFRSITE');
    end;

    FImportedByLabel                  := CreateFieldLabel(LOwner, LParent, 10, 45, 120, 21);
    FImportedByEdit                   := CreateFieldEdit(FAppModules, LOwner, LParent, 140, 45, 300, 21, 3, TRUE);
    FErrorTypeLabel                   := CreateFieldLabel(LOwner, LParent, 10, 76, 120, 21);
    FErrorTypeEdit                    := CreateFieldRichEdit(FAppModules, LOwner, LParent, 140, 76, 300, 100, 4, TRUE);
    FErrorTypeEdit.WordWrap           := True;
    FErrorTypeEdit.WantTabs           := True;
    FErrorTypeEdit.WantReturns        := True;
    FErrorTypeEdit.ScrollBars         := ssBoth;
    FErrorDescriptionLabel            := CreateFieldLabel(LOwner, LParent, 10, 186, 120, 21);
    FErrorDescriptionEdit             := CreateFieldRichEdit(FAppModules, LOwner, LParent, 140, 186, 300, 100, 5, TRUE);
    FErrorDescriptionEdit.WordWrap    := True;
    FErrorDescriptionEdit.WantTabs    := True;
    FErrorDescriptionEdit.WantReturns := True;
    FErrorDescriptionEdit.ScrollBars  := ssBoth;
    FStudyErrorsLabel                 := CreateFieldLabel(LOwner, LParent, 10, 296, 120, 21);
    FStudyErrorsComboBox              := CreateFieldComboBox(FAppModules, LOwner, LParent, 140, 296, 300, 21, 6, TRUE, csDropDownList);
    FCorrectiveActionLabel            := CreateFieldLabel(LOwner, LParent, 10, 327, 120, 21);
    FCorrectiveActionEdit             := CreateFieldRichEdit(FAppModules, LOwner, LParent, 140, 327, 300, 100, 7, TRUE);
    FCorrectiveActionEdit.WordWrap    := True;
    FCorrectiveActionEdit.WantTabs    := True;
    FCorrectiveActionEdit.WantReturns := True;
    FCorrectiveActionEdit.ScrollBars  := ssBoth;

    FFirstBtn := CreateFieldBitButton(FAppModules, LOwner, LParent, 140, 437, 50, 25, 8, TRUE, 'OUTPUTREVIEWFIRSTRECORD');
    FPriorBtn := CreateFieldBitButton(FAppModules, LOwner, LParent, 190, 437, 50, 25, 9, TRUE, 'PRIORRECORD');
    FNextBtn  := CreateFieldBitButton(FAppModules, LOwner, LParent, 240, 437, 50, 25, 10, TRUE, 'OUTPUTREVIEWNEXTRECORD');
    FLastBtn  := CreateFieldBitButton(FAppModules, LOwner, LParent, 290, 437, 50, 25, 11, TRUE, 'OUTPUTREVIEWLASTRECORD');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyMetaDataDialog.LanguageHasChanged: boolean;
const OPNAME = 'TStudyMetaDataDialog.LanguageHasChanged';
begin
  Result := True;
  try
    FImportedByLabel.Caption       := FAppModules.Language.GetString('LabelCaption.ImportedBy') + ' :';
    FErrorTypeLabel.Caption        := FAppModules.Language.GetString('LabelCaption.ErrorType') + ' :';
    FErrorDescriptionLabel.Caption := FAppModules.Language.GetString('LabelCaption.ErrorDescription') + ' :';
    FStudyErrorsLabel.Caption      := FAppModules.Language.GetString('LabelCaption.StudyErrors') + ' :';
    FCorrectiveActionLabel.Caption := FAppModules.Language.GetString('LabelCaption.CorrectiveAction') + ' :';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataDialog.Resize;
const OPNAME = 'TStudyMetaDataDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
