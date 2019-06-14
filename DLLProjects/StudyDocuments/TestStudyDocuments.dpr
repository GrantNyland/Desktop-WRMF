program TestStudyDocuments;

uses
  Forms,
  TestStudyDocumentsMain in 'TestStudyDocumentsMain.pas' {TestStudyDocumentsMainForm};

begin
  Application.Initialize;
  Application.CreateForm(TTestStudyDocumentsMainForm, TestStudyDocumentsMainForm);
  Application.Run;
end.
