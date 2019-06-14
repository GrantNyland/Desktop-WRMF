unit UStudyMetaDataValidator;

interface
uses
  Contnrs,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UStudyMetaDataDialog;

type
  TStudyMetaDataValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FIndex : integer;
    procedure CreateMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnRichEditKeyPress(ASender: TObject; var AKey: Char);
    procedure OnCreateMetaDataBtnClick(ASender : TObject);
    procedure OnDeleteMetaDataBtnClick(ASender : TObject);
    procedure OnPriorStudyMetaDataBtnClick(ASender : TObject);
    procedure OnFirstStudyMetaDataBtnClick(ASender : TObject);
    procedure OnLastStudyMetaDataBtnClick(ASender : TObject);
    procedure OnNextStudyMetaDataBtnClick(ASender : TObject);
    procedure UpdateImportedBy;
    procedure UpdateErrorType;
    procedure UpdateErrorDescription;
    procedure UpdateStudyErrors;
    procedure UpdateCorrectiveAction;
    procedure PopulateStudyErrorsComboBox;
    procedure RePopulateDataViewer;
    procedure SetFormState(ARecordEditable : boolean);
    procedure SetButtonState;
  public
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    procedure PopulateDataViewer; override;
    function StudyMetaDataDialog: TStudyMetaDataDialog;
    procedure ClearDataViewer; override;
  end;

implementation
uses
  System.UITypes,
  SysUtils,
  VCL.Graphics,
  VCL.Dialogs,
  UYieldModelDataObject,
  UStudyMetaData,
  UErrorHandlingOperations, VCL.ComCtrls, Classes;

{ TStudyMetaDataValidator }

procedure TStudyMetaDataValidator.ClearDataViewer;
const OPNAME = 'TStudyMetaDataValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with StudyMetaDataDialog do
    begin
      ImportedByEdit.Text             := '';
      ErrorTypeEdit.Lines.Text        := '';
      ErrorDescriptionEdit.Lines.Text := '';
      StudyErrorsComboBox.ItemIndex   := -1;
      CorrectiveActionEdit.Lines.Text := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.CreateMemberObjects;
const OPNAME = 'TStudyMetaDataValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TStudyMetaDataDialog.Create(FPanelOwner,FAppModules);
    with StudyMetaDataDialog do
    begin
      ImportedByEdit.FieldProperty       := FAppModules.FieldProperties.FieldProperty('ImportedBy');
      ImportedByEdit.OnEnter             := OnEditControlEnter;
      ImportedByEdit.OnExit              := OnEditControltExit;

      ErrorTypeEdit.FieldProperty        := FAppModules.FieldProperties.FieldProperty('ErrorType');
      ErrorTypeEdit.OnEnter              := OnEditControlEnter;
      ErrorTypeEdit.OnExit               := OnEditControltExit;
      ErrorTypeEdit.OnKeyPress           := OnRichEditKeyPress;

      ErrorDescriptionEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('ErrorDescription');
      ErrorDescriptionEdit.OnEnter       := OnEditControlEnter;
      ErrorDescriptionEdit.OnExit        := OnEditControltExit;
      ErrorDescriptionEdit.OnKeyPress    := OnRichEditKeyPress;

      StudyErrorsComboBox.FieldProperty  := FAppModules.FieldProperties.FieldProperty('StudyErrors');
      StudyErrorsComboBox.OnEnter        := OnEditControlEnter;
      StudyErrorsComboBox.OnExit         := OnEditControltExit;

      CorrectiveActionEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('CorrectiveAction');
      CorrectiveActionEdit.OnEnter       := OnEditControlEnter;
      CorrectiveActionEdit.OnExit        := OnEditControltExit;
      CorrectiveActionEdit.OnKeyPress    := OnRichEditKeyPress;

      CreateBtn.OnClick := OnCreateMetaDataBtnClick;
      DeleteBtn.OnClick := OnDeleteMetaDataBtnClick;
      PriorBtn.OnClick  := OnPriorStudyMetaDataBtnClick;
      FirstBtn.OnClick  := OnFirstStudyMetaDataBtnClick;
      LastBtn.OnClick   := OnLastStudyMetaDataBtnClick;
      NextBtn.OnClick   := OnNextStudyMetaDataBtnClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyMetaDataValidator.LanguageHasChanged: boolean;
const OPNAME = 'TStudyMetaDataValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.StudyMetadata');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TStudyMetaDataValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TStudyMetaDataValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if((Sender = StudyMetaDataDialog.ImportedByEdit) AND
       (StudyMetaDataDialog.ImportedByEdit.HasValueChanged)) then
      UpdateImportedBy;
    if((Sender = StudyMetaDataDialog.ErrorTypeEdit) AND
       (StudyMetaDataDialog.ErrorTypeEdit.HasValueChanged)) then
      UpdateErrorType;
    if((Sender = StudyMetaDataDialog.ErrorDescriptionEdit) AND
       (StudyMetaDataDialog.ErrorDescriptionEdit.HasValueChanged)) then
      UpdateErrorDescription;
    if((Sender = StudyMetaDataDialog.StudyErrorsComboBox) AND
       (StudyMetaDataDialog.StudyErrorsComboBox.HasValueChanged)) then
      UpdateStudyErrors;
    if((Sender = StudyMetaDataDialog.CorrectiveActionEdit) AND
       (StudyMetaDataDialog.CorrectiveActionEdit.HasValueChanged)) then
      UpdateCorrectiveAction;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.OnCreateMetaDataBtnClick(ASender: TObject);
const OPNAME = 'TStudyMetaDataValidator.OnCreateMetaDataBtnClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.CreateNewStudyMetaData;
    FIndex := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.StudyMetaDataCount - 1;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.OnDeleteMetaDataBtnClick(ASender: TObject);
const OPNAME = 'TStudyMetaDataValidator.OnDeleteMetaDataBtnClick';
begin
  try
    TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.DeleteStudyErrorByID[FIdentifier];
    if(FIndex > 0) then
      FIndex := FIndex - 1;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.PopulateDataViewer;
const OPNAME = 'TStudyMetaDataValidator.PopulateDataViewer';
begin
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.PopulateStudyErrorsComboBox;
const OPNAME = 'TStudyMetaDataValidator.PopulateStudyErrorsComboBox';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to StudyMetaDataDialog.StudyErrorsComboBox.Items.Count - 1 do
      StudyMetaDataDialog.StudyErrorsComboBox.Items.Clear; 
    StudyMetaDataDialog.StudyErrorsComboBox.Items.AddObject(FAppModules.Language.GetString('TStudyMetaDataValidator.DataError'),TObject(1));
    StudyMetaDataDialog.StudyErrorsComboBox.Items.AddObject(FAppModules.Language.GetString('TStudyMetaDataValidator.DrawingError'),TObject(2));
    StudyMetaDataDialog.StudyErrorsComboBox.Items.AddObject(FAppModules.Language.GetString('TStudyMetaDataValidator.WRMFError'),TObject(3));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.RePopulateDataViewer;
const OPNAME = 'TStudyMetaDataValidator.RePopulateDataViewer';
var
  LStudyMetadata     : TStudyMetaData;
  LStudyMetaDataList : TStudyMetaDataList;
begin
  try
    PopulateStudyErrorsComboBox;
    LStudyMetaDataList := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList; 
    if(LStudyMetaDataList.StudyMetaDataCount > 0) then
    begin 
      Self.Identifier := -1;
      Self.Identifier := LStudyMetaDataList.CastStudyMetaDataByIndex[FIndex].Identifier;
      LStudyMetadata  := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.CastStudyMetaDataByID[FIdentifier];
      StudyMetaDataDialog.DeleteBtn.Enabled := not LStudyMetadata.RecordReadOnly;
      SetFormState(LStudyMetadata.RecordReadOnly);
      if(LStudyMetadata <> nil) then
      begin
        StudyMetaDataDialog.ImportedByEdit.Text             := LStudyMetadata.ImportedBy;
        StudyMetaDataDialog.ErrorTypeEdit.Lines.Text        := LStudyMetadata.ErrorType;
        StudyMetaDataDialog.ErrorDescriptionEdit.Lines.Text := LStudyMetadata.ErrorDescription;
        StudyMetaDataDialog.StudyErrorsComboBox.ItemIndex   := StudyMetaDataDialog.StudyErrorsComboBox.Items.IndexOfObject(TObject(LStudyMetadata.StudyErrors));
        StudyMetaDataDialog.CorrectiveActionEdit.Lines.Text := LStudyMetadata.CorrectiveAction;
      end;
      SetButtonState;
    end
    else
    begin
      SetFormState(True);
      StudyMetaDataDialog.PriorBtn.Enabled := False;
      StudyMetaDataDialog.FirstBtn.Enabled := False;
      StudyMetaDataDialog.LastBtn.Enabled  := False;
      StudyMetaDataDialog.NextBtn.Enabled  := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyMetaDataValidator.StudyHasChanged: boolean;
const OPNAME = 'TStudyMetaDataValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
   Initialise;
   PopulateDataViewer;
   LanguageHasChanged;
   Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStudyMetaDataValidator.StudyMetaDataDialog: TStudyMetaDataDialog;
const OPNAME = 'TStudyMetaDataValidator.StudyMetaDataDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TStudyMetaDataDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.OnFirstStudyMetaDataBtnClick(ASender: TObject);
const OPNAME = 'TStudyMetaDataValidator.OnFirstStudyMetaDataBtnClick';
begin
  try
    FIndex := 0;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.OnLastStudyMetaDataBtnClick(ASender: TObject);
const OPNAME = 'TStudyMetaDataValidator.OnLastStudyMetaDataBtnClick';
begin
  try
    FIndex := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.StudyMetaDataCount - 1;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.OnNextStudyMetaDataBtnClick(ASender: TObject);
const OPNAME = 'TStudyMetaDataValidator.OnNextStudyMetaDataBtnClick';
begin
  try
    FIndex := FIndex + 1;
    if(FIndex < TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.StudyMetaDataCount) then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.OnPriorStudyMetaDataBtnClick(ASender: TObject);
const OPNAME = 'TStudyMetaDataValidator.OnPriorStudyMetaDataBtnClick';
begin
  try
    FIndex := FIndex - 1;
    if(FIndex >= 0) then
      PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.UpdateCorrectiveAction;
const OPNAME = 'TStudyMetaDataValidator.UpdateCorrectiveAction';
var
  LStudyMetaData    : TStudyMetaData;
  LErrorMessage     : string;
  LCorrectiveAction : Widestring;
  LIndex            : integer;
begin
  try
    LStudyMetaData := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.CastStudyMetaDataByID[FIdentifier];
    if(LStudyMetaData <> nil) then
    begin
      with StudyMetaDataDialog do
      begin
        for LIndex := 0 to CorrectiveActionEdit.Lines.Count - 1 do
          LCorrectiveAction := LCorrectiveAction + Trim(CorrectiveActionEdit.Lines[Lindex]) + ' ';
        if(Length(LCorrectiveAction) > 255) then
          LCorrectiveAction := Copy(LCorrectiveAction,0,255);

        if(FAppModules.FieldProperties.ValidateFieldProperty(
            CorrectiveActionEdit.FieldProperty.FieldName,LCorrectiveAction,LErrorMessage)) then
        begin
          LStudyMetaData.CorrectiveAction := LCorrectiveAction;
          CorrectiveActionEdit.SetFieldValue(LStudyMetaData.CorrectiveAction);
        end;
        CorrectiveActionEdit.ContextValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.UpdateErrorDescription;
const OPNAME = 'TStudyMetaDataValidator.UpdateErrorDescription';
var
  LStudyMetaData : TStudyMetaData;
  LErrorMessage  : string;
  LDescription   : Widestring;
  LIndex         : integer; 
begin
  try
    LStudyMetaData := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.CastStudyMetaDataByID[FIdentifier];
    if(LStudyMetaData <> nil) then
    begin
      with StudyMetaDataDialog do
      begin
        for LIndex := 0 to ErrorDescriptionEdit.Lines.Count - 1 do
          LDescription := LDescription + Trim(ErrorDescriptionEdit.Lines[LIndex]) + ' ';
        if(Length(LDescription) > 255) then
          LDescription := Copy(LDescription,0,255);
        if(FAppModules.FieldProperties.ValidateFieldProperty(
            ErrorDescriptionEdit.FieldProperty.FieldName,LDescription,LErrorMessage)) then
        begin
          LStudyMetaData.ErrorDescription := Trim(LDescription);
          ErrorDescriptionEdit.SetFieldValue(LStudyMetaData.ErrorDescription);
        end;
        ErrorDescriptionEdit.ContextValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.UpdateErrorType;
const OPNAME = 'TStudyMetaDataValidator.UpdateErrorType';
var
  LStudyMetaData : TStudyMetaData;
  LErrorMessage  : string;
  LErrorType     : Widestring;
  LIndex         : integer; 
begin
  try
    LStudyMetaData := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.CastStudyMetaDataByID[FIdentifier];
    if(LStudyMetaData <> nil) then
    begin
      with StudyMetaDataDialog do
      begin
        for LIndex := 0 to ErrorTypeEdit.Lines.Count - 1 do
          LErrorType := LErrorType + Trim(ErrorTypeEdit.Lines[LIndex]) + ' ';
        if(Length(LErrorType) > 255) then
          LErrorType := Copy(LErrorType,0,255);
        if(FAppModules.FieldProperties.ValidateFieldProperty(
            ErrorTypeEdit.FieldProperty.FieldName,LErrorType,LErrorMessage)) then
        begin
          LStudyMetaData.ErrorType := Trim(LErrorType);
          ErrorTypeEdit.SetFieldValue(LStudyMetaData.ErrorType);
        end;
        ErrorTypeEdit.ContextValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.UpdateImportedBy;
const OPNAME = 'TStudyMetaDataValidator.UpdateImportedBy';
var
  LStudyMetaData : TStudyMetaData;
  LErrorMessage  : string;
begin
  try
    LStudyMetaData := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.CastStudyMetaDataByID[FIdentifier];
    if(LStudyMetaData <> nil) then
    begin
      with StudyMetaDataDialog do
      begin
        if(FAppModules.FieldProperties.ValidateFieldProperty(
            ImportedByEdit.FieldProperty.FieldName,ImportedByEdit.Text,LErrorMessage)) then
        begin
          LStudyMetaData.ImportedBy := Trim(ImportedByEdit.Text);
          ImportedByEdit.SetFieldValue(LStudyMetaData.ImportedBy);
        end;
        ImportedByEdit.ContextValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.UpdateStudyErrors;
const OPNAME = 'TStudyMetaDataValidator.UpdateStudyErrors';
var
  LStudyMetaData : TStudyMetaData;
  LErrorMessage  : string;
  LStudyError    : integer;
begin
  try
    LStudyMetaData := TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.CastStudyMetaDataByID[FIdentifier];
    if(LStudyMetaData <> nil) then
    begin
      with StudyMetaDataDialog do
      begin
        LStudyError := integer(StudyMetaDataDialog.StudyErrorsComboBox.Items.Objects[StudyMetaDataDialog.StudyErrorsComboBox.ItemIndex]);
        if(FAppModules.FieldProperties.ValidateFieldProperty(
            StudyErrorsComboBox.FieldProperty.FieldName,IntToStr(LStudyError),LErrorMessage)) then
        begin
          LStudyMetaData.StudyErrors := LStudyError;
          StudyErrorsComboBox.Items.IndexOfObject(TObject(LStudyError));
        end;
        StudyErrorsComboBox.ValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.SetFormState(ARecordEditable : boolean);
const OPNAME = 'TStudyMetaDataValidator.SetFormState';
begin
  try
    if ARecordEditable then
    begin
      with StudyMetaDataDialog do
      begin
        ImportedByEdit.ReadOnly       := True;
        ImportedByEdit.Color          := clBtnFace;
        ErrorTypeEdit.ReadOnly        := True;
        ErrorTypeEdit.Color           := clBtnFace;
        ErrorDescriptionEdit.ReadOnly := True;
        ErrorDescriptionEdit.Color    := clBtnFace;
        StudyErrorsComboBox.IsEnabled := False;
        StudyErrorsComboBox.Color     := clBtnFace;
        CorrectiveActionEdit.ReadOnly := True;
        CorrectiveActionEdit.Color    := clBtnFace;
      end;
    end
    else
    begin
      with StudyMetaDataDialog do
      begin
        ImportedByEdit.ReadOnly       := False;
        ImportedByEdit.Color          := clWindow;
        ErrorTypeEdit.ReadOnly        := False;
        ErrorTypeEdit.Color           := clWindow;
        ErrorDescriptionEdit.ReadOnly := False;
        ErrorDescriptionEdit.Color    := clWindow;
        StudyErrorsComboBox.IsEnabled := True;
        StudyErrorsComboBox.Color     := clWindow;
        CorrectiveActionEdit.ReadOnly := False;
        CorrectiveActionEdit.Color    := clWindow;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.SetButtonState;
const OPNAME = 'TStudyMetaDataValidator.SetButtonState';
begin
  try
    StudyMetaDataDialog.PriorBtn.Enabled := (FIndex > 0);
    StudyMetaDataDialog.FirstBtn.Enabled := (FIndex <> 0) AND (FIndex > 0);
    StudyMetaDataDialog.LastBtn.Enabled  := (FIndex <> TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.StudyMetaDataCount - 1);
    StudyMetaDataDialog.NextBtn.Enabled  := (FIndex < TYieldModelDataObject(FAppModules.Model.ModelData).StudyMetaDataList.StudyMetaDataCount - 1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStudyMetaDataValidator.OnRichEditKeyPress(ASender: TObject; var AKey: Char);
const OPNAME = 'TStudyMetaDataValidator.OnRichEditKeyPress';
var
  LCount       : integer;
  LDescription : string; 
begin
  try
    for LCount := 0 to TFieldRichEdit(ASender).Lines.Count - 1 do
      LDescription := LDescription + Trim(TFieldRichEdit(ASender).Lines[LCount]) + ' ';  
    LCount := Length(LDescription);
    if(LCount >= 255) then
      MessageDlg(FAppModules.Language.GetString('Message.MaxCharactersReached'),mtWarning,[mbOK], 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

