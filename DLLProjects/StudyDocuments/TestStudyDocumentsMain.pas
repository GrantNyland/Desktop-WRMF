unit TestStudyDocumentsMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  UStudyDocumentManager, UAbstractObject, UWordDocumentLauncher, UPDFDocumentLauncher;

type
  TTestStudyDocumentsMainForm = class(TForm)
    FileNameEdit: TEdit;
    BookMarkEdit: TEdit;
    PageNumberEdit: TEdit;
    LaunchButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LaunchButtonClick(Sender: TObject);
  private
    FStudyDocumentManager: TStudyDocumentManager;
  public
  end;

var
  TestStudyDocumentsMainForm: TTestStudyDocumentsMainForm;

implementation

{$R *.dfm}

procedure TTestStudyDocumentsMainForm.FormCreate(Sender: TObject);
const OPNAME = 'TTestStudyDocumentsMainForm.FormCreate';
begin
  FStudyDocumentManager := TStudyDocumentManager.Create(nil);
end;

procedure TTestStudyDocumentsMainForm.FormDestroy(Sender: TObject);
const OPNAME = 'TTestStudyDocumentsMainForm.FormDestroy';
begin
  FreeAndNil(FStudyDocumentManager);
end;

procedure TTestStudyDocumentsMainForm.LaunchButtonClick(Sender: TObject);
const OPNAME = 'TTestStudyDocumentsMainForm.LaunchButtonClick';
begin
  TPDFDocumentLauncher.Launch(FileNameEdit.Text, StrToInt(PageNumberEdit.Text));
end;

end.
