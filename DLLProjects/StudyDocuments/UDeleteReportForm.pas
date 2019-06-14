//
//  UNIT      : Contains TfrmDeleteReportForm Class
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/02/02
//  COPYRIGHT : Copyright © 2005 DWAF
//

unit UDeleteReportForm;

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
  TfrmDeleteReportForm = class(TAbstractForm)
    dlgFileSelector: TOpenDialog;
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
    lblReportType: TLabel;
    cmbReportType: TComboBox;
    cmbFileName: TComboBox;
    procedure cmbReportTypeChange(Sender: TObject);
    procedure cmbFileNameChange(Sender: TObject);
    procedure cmbDocCategoryChange(Sender: TObject);
  private
  protected
    FDocumentIdentifiers: TStudyDocumentDetailList;
    procedure SetOKButtonState;
  public
    function LanguageHasChanged: boolean; override;
    function PopulateDocumentDetail(ADocumentDetail:TStudyDocumentDetail): boolean;
    function PopulateDialog(ADocumentIdentifiers: TStudyDocumentDetailList): boolean;
    property DocumentIdentifiers: TStudyDocumentDetailList read FDocumentIdentifiers;
  end;

implementation

uses
  UUtilities,
  UDataSetType,
  UErrorHandlingOperations;

{$R *.dfm}

function TfrmDeleteReportForm.LanguageHasChanged: boolean;
const OPNAME = 'TfrmDeleteReportForm.LanguageHasChanged';
begin
  Result := False;
  try
    cmbReportType.Items.Clear;
    cmbReportType.Items.Add('Word Documents (*.doc)');
    cmbReportType.Items.Add('Acrobat Documents (*.pdf)');
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TfrmDeleteReportForm.PopulateDocumentDetail(ADocumentDetail: TStudyDocumentDetail): boolean;
const OPNAME = 'TfrmDeleteReportForm.PopulateDocumentDetail';
begin
  Result := False;
  try
    if Assigned(ADocumentDetail) then
    begin
      ADocumentDetail.CategoryKey   := cmbDocCategory.Text;
      ADocumentDetail.IdentifierKey := edtID.Text;
      ADocumentDetail.Filename      := cmbFileName.Text;
      ADocumentDetail.MenuCaption   := edtMenuCaption.Text;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TfrmDeleteReportForm.PopulateDialog(ADocumentIdentifiers: TStudyDocumentDetailList): boolean;
const OPNAME = 'TfrmDeleteReportForm.PopulateDialog';
var
  LCategory       : TStringList;
  LIndex: integer;
begin
  Result := False;
  try
    FDocumentIdentifiers := ADocumentIdentifiers;
    if Assigned(FDocumentIdentifiers) then
    begin
      LCategory       := TStringList.Create;
      try
        LCategory.Sorted := True;
        LCategory.Duplicates := dupIgnore;
        for LIndex := 0 to FDocumentIdentifiers.Count -1 do
          LCategory.Add(FDocumentIdentifiers.DocumentByIndex[LIndex].CategoryKey);
        cmbDocCategory.Items.Assign(LCategory);
        cmbReportType.ItemIndex := 0;
        cmbDocCategory.ItemIndex := 0;
        cmbDocCategoryChange(cmbDocCategory);
      finally
        FreeAndNil(LCategory);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmDeleteReportForm.SetOKButtonState;
const OPNAME = 'TfrmDeleteReportForm.SetOKButtonState';
begin
  try
    btnOk.Enabled := (Trim(cmbDocCategory.Text) <> '') and
                     (Trim(cmbReportType.Text) <> '') and
                     (Trim(edtID.Text) <> '') and
                     (Trim(cmbFileName.Text) <> '') and
                     (Trim(edtMenuCaption.Text) <> '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmDeleteReportForm.cmbReportTypeChange(Sender: TObject);
const OPNAME = 'TfrmDeleteReportForm.cmbReportTypeChange';
var
  LIndex: integer;
  LReportType: TReportTypes;
begin
  try
    cmbFileName.Items.Clear;
    cmbFileName.Text := '';
    edtMenuCaption.Text := '';
    edtID.Text          := '';

    LReportType := rtNotSupported;
    case cmbReportType.ItemIndex of
      0 :LReportType := rtAcrobat;
      1 :LReportType := rtWord;
    end;//case
    cmbFileName.Items.Clear;
    if(Trim(cmbDocCategory.Text) <> '') then
    begin
      for LIndex := 0 to FDocumentIdentifiers.Count-1 do
      begin
        if(FDocumentIdentifiers.DocumentByIndex[LIndex].ReportType = LReportType) and
          (FDocumentIdentifiers.DocumentByIndex[LIndex].CategoryKey = Trim(cmbDocCategory.Text)) then
           cmbFileName.Items.Add(FDocumentIdentifiers.DocumentByIndex[LIndex].Filename);
      end;
      if(cmbFileName.Items.Count > 0) then
      begin
        cmbFileName.ItemIndex := 0;
        cmbFileName.Text := cmbFileName.Items[cmbFileName.ItemIndex];
        cmbFileName.OnChange(cmbFileName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmDeleteReportForm.cmbFileNameChange(Sender: TObject);
const OPNAME = 'TfrmDeleteReportForm.cmbFileNameChange';
var
  LDocumentDetail: TStudyDocumentDetail;
begin
  try
    edtMenuCaption.Text := '';
    edtID.Text          := '';

    LDocumentDetail := FDocumentIdentifiers.GetDocument(Trim(cmbDocCategory.Text),Trim(cmbFileName.Text));
    if Assigned(LDocumentDetail) then
    begin
      edtMenuCaption.Text := LDocumentDetail.MenuCaption;
      edtID.Text          := LDocumentDetail.IdentifierKey;
    end;
    SetOKButtonState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TfrmDeleteReportForm.cmbDocCategoryChange(Sender: TObject);
const OPNAME = 'TfrmDeleteReportForm.cmbDocCategoryChange';
var
  LIndex: integer;
  LReportType: TReportTypes;
begin
  try
    cmbFileName.Items.Clear;
    cmbFileName.Text := '';
    edtMenuCaption.Text := '';
    edtID.Text          := '';

    LReportType := rtNotSupported;
    case cmbReportType.ItemIndex of
      0 :LReportType := rtAcrobat;
      1 :LReportType := rtWord;
    end;//case

    if(Trim(cmbDocCategory.Text) <> '') then
    begin
      for LIndex := 0 to FDocumentIdentifiers.Count-1 do
      begin
        if(FDocumentIdentifiers.DocumentByIndex[LIndex].ReportType = LReportType) and
          (FDocumentIdentifiers.DocumentByIndex[LIndex].CategoryKey = Trim(cmbDocCategory.Text)) then
           cmbFileName.Items.Add(FDocumentIdentifiers.DocumentByIndex[LIndex].Filename);
      end;
      if(cmbFileName.Items.Count > 0) then
      begin
        cmbFileName.ItemIndex := 0;
        cmbFileName.Text := cmbFileName.Items[cmbFileName.ItemIndex];
        cmbFileName.OnChange(cmbFileName);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
