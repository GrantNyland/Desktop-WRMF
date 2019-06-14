//
//  UNIT      : Contains TfrmEditReportForm Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/02/02
//  COPYRIGHT : Copyright © 2005 DWAF
//

unit UEditReportForm;

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
  TfrmEditReportForm = class(TAbstractForm)
    pnlButtons: TPanel;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    pnlStudy: TPanel;
    lblDocCategoryCaption: TLabel;
    lblDocNameCaption: TLabel;
    cmbDocCategory: TComboBox;
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
    cmbFileName: TComboBox;
    procedure edtFileNameChange(Sender: TObject);
    procedure edtPageNumberKeyPress(Sender: TObject; var Key: Char);
    procedure cmbDocCategoryChange(Sender: TObject);
    procedure cmbFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  protected
    FDocumentIdentifiers: TStudyDocumentDetailList;
    FFileNames: TStringList;
    procedure SetOKButtonState;
    function GetReportType: TReportTypes;
  public
    function LanguageHasChanged: boolean; override;
    function PopulateDocumentDetail(ADocumentDetail:TStudyDocumentDetail): boolean;
    function PopulateDialog(ACategory: TStrings;ADocumentDetail:TStudyDocumentDetail): boolean;
    property DocumentIdentifiers: TStudyDocumentDetailList read FDocumentIdentifiers write FDocumentIdentifiers;
  end;

implementation

uses
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

{$R *.dfm}

procedure TfrmEditReportForm.FormCreate(Sender: TObject);
const OPNAME = 'TfrmEditReportForm.FormCreate';
begin
  try
    FFileNames := TStringList.Create;
    FFileNames.Sorted := True;
    FFileNames.Duplicates := dupIgnore;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmEditReportForm.FormDestroy(Sender: TObject);
const OPNAME = 'TfrmEditReportForm.FormDestroy';
begin
  try
    FreeAndNil(FFileNames);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmEditReportForm.LanguageHasChanged: boolean;
const OPNAME = 'TfrmEditReportForm.LanguageHasChanged';
begin
  Result := False;
  try
    cmbReportType.Items.Clear;
    cmbReportType.Items.Add('Word Documents (*.doc)');
    cmbReportType.Items.Add('Acrobat Documents (*.pdf)');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TfrmEditReportForm.PopulateDocumentDetail(ADocumentDetail: TStudyDocumentDetail): boolean;
const OPNAME = 'TfrmEditReportForm.PopulateDocumentDetail';
begin
  Result := False;
  try
    if Assigned(ADocumentDetail) then
    begin
      ADocumentDetail.CategoryKey   := Trim(cmbDocCategory.Text);
      ADocumentDetail.IdentifierKey := Trim(edtID.Text);
      ADocumentDetail.Filename      := Trim(cmbFileName.Text);
      ADocumentDetail.MenuCaption   := Trim(edtMenuCaption.Text);
      ADocumentDetail.BookMark      := Trim(edtBookMark.Text);
      ADocumentDetail.PageNumber    := StrToInt(Trim(edtPageNumber.Text));
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmEditReportForm.PopulateDialog(ACategory: TStrings;ADocumentDetail: TStudyDocumentDetail): boolean;
const OPNAME = 'TfrmEditReportForm.PopulateDialog';
begin
  Result := False;
  try
    if Assigned(ACategory) and Assigned(ADocumentDetail) then
    begin
      cmbDocCategory.Items.Assign(ACategory);
      cmbFileName.Text    := ADocumentDetail.Filename;
      cmbFileName.ItemIndex := cmbFileName.Items.IndexOf(ADocumentDetail.Filename);
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
      cmbDocCategoryChange(nil);
      btnOk.Enabled := False;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmEditReportForm.SetOKButtonState;
const OPNAME = 'TfrmEditReportForm.SetOKButtonState';
begin
  try
    btnOk.Enabled := (Trim(cmbDocCategory.Text) <> '') and
                     (Trim(cmbReportType.Text) <> '') and
                     (Trim(edtID.Text) <> '') and
                     (Trim(cmbFileName.Text) <> '') and
                     (Trim(edtMenuCaption.Text) <> '') and
                     ((Trim(edtBookMark.Text) <> '') or
                     (Trim(edtPageNumber.Text) <> ''));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmEditReportForm.edtFileNameChange(Sender: TObject);
const OPNAME = 'TfrmEditReportForm.edtFileNameChange';
begin
  try
    SetOKButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmEditReportForm.edtPageNumberKeyPress(Sender: TObject;var Key: Char);
const OPNAME = 'TfrmEditReportForm.edtPageNumberKeyPress';
begin
  try
    if (not CharInSet(Key,['0'..'9',#8])) then
      Key := #0;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmEditReportForm.cmbDocCategoryChange(Sender: TObject);
const OPNAME = 'TfrmEditReportForm.cmbDocCategoryChange';
begin
  try

    edtMenuCaption.Text := '';
    edtID.Text          := '';
    edtBookMark.Text    := '';
    edtPageNumber.Text  := '';
    if FDocumentIdentifiers.GetDocumentFileNames(GetReportType,Trim(cmbDocCategory.Text),FFileNames) then
    begin
      cmbFileName.Items.Assign(FFileNames);
      if(cmbFileName.Items.Count > 0) then
      begin
        cmbFileName.ItemIndex := 0;
        cmbFileName.Text := cmbFileName.Items[0];
        cmbFileNameChange(nil);
      end;
    end;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmEditReportForm.cmbFileNameChange(Sender: TObject);
const OPNAME = 'TfrmEditReportForm.cmbFileNameChange';
var
  LStudyDocumentDetail: TStudyDocumentDetail;
begin
  try
    edtMenuCaption.Text := '';
    edtID.Text          := '';
    edtBookMark.Text    := '';
    edtPageNumber.Text  := '';

    LStudyDocumentDetail := FDocumentIdentifiers.GetDocument(Trim(cmbDocCategory.Text),Trim(cmbFileName.Text));
    if Assigned(LStudyDocumentDetail) then
    begin
      edtMenuCaption.Text := LStudyDocumentDetail.MenuCaption;
      edtID.Text          := LStudyDocumentDetail.IdentifierKey;
      edtBookMark.Text    := LStudyDocumentDetail.BookMark;
      edtPageNumber.Text  := IntToStr(LStudyDocumentDetail.PageNumber);
    end;

  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmEditReportForm.GetReportType: TReportTypes;
const OPNAME = 'TfrmEditReportForm.GetReportType';
begin
  Result := rtNotSupported;
  try
    case cmbReportType.ItemIndex of
      0:  Result := rtAcrobat;
      1:  Result := rtWord;
    end;
  // Handle Exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
