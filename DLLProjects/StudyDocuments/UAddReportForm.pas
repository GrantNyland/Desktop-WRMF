//
//  UNIT      : Contains TfrmAddReportForm Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/02/02
//  COPYRIGHT : Copyright © 2005 DWAF
//

unit UAddReportForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,

  UAbstractComponent,
  UAbstractModelObjects,
  UHelpContexts,
  UStudyObjects,
  UAbstractObject,
  UStudyDocumentMenuItemManager;

type
  TfrmAddReportForm = class(TAbstractForm)
    dlgFileSelector: TOpenDialog;
    pnlButtons: TPanel;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    pnlStudy: TPanel;
    lblDocCategoryCaption: TLabel;
    lblDocNameCaption: TLabel;
    edtFileName: TEdit;
    cmbDocCategory: TComboBox;
    btnSelectFile: TButton;
    lblMenuCaption: TLabel;
    edtMenuCaption: TEdit;
    lblID: TLabel;
    edtID: TEdit;
    lblBookMark: TLabel;
    edtBookMark: TEdit;
    lblPageNumber: TLabel;
    edtPageNumber: TEdit;
    lblReportType: TLabel;
    cmbReportType: TComboBox;
    procedure edtFileNameChange(Sender: TObject);
    procedure edtPageNumberKeyPress(Sender: TObject; var Key: Char);
    procedure btnSelectFileClick(Sender: TObject);
  private
  protected
    FDocumentIdentifiers: TStudyDocumentDetailList;
    procedure SetOKButtonState;
    function ReportAlreadyAxists(AFileName: string): boolean;
    function CopyFileToReportsDir(AFileName: string): boolean;
  public
    function LanguageHasChanged: boolean; override;
    function PopulateDocumentDetail(ADocumentDetail:TStudyDocumentDetail): boolean;
    function PopulateDialog(ACategory: TStrings;ADocumentDetail:TStudyDocumentDetail): boolean;
    property DocumentIdentifiers: TStudyDocumentDetailList read FDocumentIdentifiers write FDocumentIdentifiers;
  end;

implementation

uses
  System.UITypes,
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

{$R *.dfm}

function TfrmAddReportForm.LanguageHasChanged: boolean;
const OPNAME = 'TfrmAddReportForm.LanguageHasChanged';
begin
  Result := False;
  try
    cmbReportType.Items.Clear;
    cmbReportType.Items.Add('Word Documents (*.doc)');
    cmbReportType.Items.Add('Acrobat Documents (*.pdf)');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TfrmAddReportForm.PopulateDocumentDetail(ADocumentDetail: TStudyDocumentDetail): boolean;
const OPNAME = 'TfrmAddReportForm.PopulateDocumentDetail';
begin
  Result := False;
  try
    if Assigned(ADocumentDetail) then
    begin
      ADocumentDetail.CategoryKey   := Trim(cmbDocCategory.Text);
      ADocumentDetail.IdentifierKey := Trim(edtID.Text);
      ADocumentDetail.Filename      := Trim(edtFileName.Text);
      ADocumentDetail.MenuCaption   := Trim(edtMenuCaption.Text);
      ADocumentDetail.BookMark      := Trim(edtBookMark.Text);
      ADocumentDetail.PageNumber    := StrToInt(Trim(edtPageNumber.Text));
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmAddReportForm.PopulateDialog(ACategory: TStrings;ADocumentDetail: TStudyDocumentDetail): boolean;
const OPNAME = 'TfrmAddReportForm.PopulateDialog';
begin
  Result := False;
  try
    if Assigned(ACategory) and Assigned(ADocumentDetail) then
    begin
      cmbDocCategory.Items.Assign(ACategory);
      edtFileName.Text    := ADocumentDetail.Filename;
      edtMenuCaption.Text := ADocumentDetail.MenuCaption;
      edtID.Text          := ADocumentDetail.IdentifierKey;
      edtBookMark.Text    := ADocumentDetail.BookMark;
      edtPageNumber.Text  := IntToStr(ADocumentDetail.PageNumber);

      cmbReportType.ItemIndex := 0;
      cmbDocCategory.ItemIndex := 0;
      case ADocumentDetail.ReportType of
        rtAcrobat: cmbReportType.ItemIndex := 0;
        rtWord   : cmbReportType.ItemIndex := 1;
      end;//case

      if(cmbDocCategory.Items.IndexOf(ADocumentDetail.CategoryKey) > 0) then
        cmbDocCategory.ItemIndex := cmbDocCategory.Items.IndexOf(ADocumentDetail.CategoryKey);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmAddReportForm.SetOKButtonState;
const OPNAME = 'TfrmAddReportForm.SetOKButtonState';
begin
  try
    btnOk.Enabled := (Trim(cmbDocCategory.Text) <> '') and
                     (Trim(cmbReportType.Text) <> '') and
                     (Trim(edtID.Text) <> '') and
                     (Trim(edtFileName.Text) <> '') and
                     (Trim(edtMenuCaption.Text) <> '') and
                     ((Trim(edtBookMark.Text) <> '') or
                     (Trim(edtPageNumber.Text) <> ''));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmAddReportForm.edtFileNameChange(Sender: TObject);
const OPNAME = 'TfrmAddReportForm.edtFileNameChange';
begin
  try

    SetOKButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmAddReportForm.edtPageNumberKeyPress(Sender: TObject;var Key: Char);
const OPNAME = 'TfrmAddReportForm.edtPageNumberKeyPress';
begin
  try
    if (not CharInSet(Key, ['0'..'9',#8])) then
      Key := #0;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmAddReportForm.btnSelectFileClick(Sender: TObject);
const OPNAME = 'TfrmAddReportForm.btnSelectFileClick';
var
  LPath: string;
  LLongFileName,
  LFileName: string;
  LFileNoExt: string;
begin
  try

    LPath := ExtractFilePath(ApplicationExeName) + 'Reports\'+FAppModules.StudyArea.StudyAreaCode + '\'+
             Trim(cmbDocCategory.Text);
    dlgFileSelector.InitialDir := LPath;
    case cmbReportType.ItemIndex of
      0: dlgFileSelector.Filter := 'Acrobat Documents (*.pdf)|*.pdf';
      1: dlgFileSelector.Filter := 'Word Documents (*.doc)|*.doc';
    end;//case
    if dlgFileSelector.Execute then
    begin
      LFileName     := ExtractFileName(dlgFileSelector.FileName);
      LLongFileName := dlgFileSelector.FileName;
      if ReportAlreadyAxists(LFileName) then
      begin
        ShowMessage('Report ('+LFileName+') already exists. Please select an new one.');
      end
      else
      begin
        if CopyFileToReportsDir(dlgFileSelector.FileName) then
        begin
          LFileNoExt          := Copy(LFileName,1,Length(LFileName)-4);
          edtFileName.Text    := LFileName;
          edtMenuCaption.Text := LFileNoExt;
          LFileNoExt          := StringReplace(LFileNoExt,' ','',[rfReplaceAll]);
          edtID.Text          := LFileNoExt;
          edtBookMark.Text    := '';
          edtPageNumber.Text  := '1';
        end;
      end;
    end;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmAddReportForm.ReportAlreadyAxists(AFileName: string): boolean;
const OPNAME = 'TfrmAddReportForm.ReportAlreadyAxists';
begin
  Result := False;
  try
    Result := (FDocumentIdentifiers.GetDocument(Trim(cmbDocCategory.Text),AFileName) <> nil);
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmAddReportForm.CopyFileToReportsDir(AFileName: string): boolean;
const OPNAME = 'TfrmAddReportForm.CopyFileToReportsDir';
      CopyFilesMsg = 'The report is not sitting in the application reports directory and will be copied there. Do you want to continue?';
var
  LNewFileName: string;
begin
  Result := False;
  try

    LNewFileName := GetReportFileName(FAppModules.StudyArea.StudyAreaCode,Trim(cmbDocCategory.Text),ExtractFileName(AFileName));
    if(UpperCase(AFileName) <> UpperCase(LNewFileName)) then
    begin
      if (MessageDlg(CopyFilesMsg,mtConfirmation,mbOKCancel,0) = mrOk) then
      begin
        Result := CopyFile(PChar(AFileName),PChar(LNewFileName),True);
      end;
    end
    else
      Result := True;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;

end;

end.
