program TestCOM;

uses
  Forms,
  UTestCOMMainForm in 'UTestCOMMainForm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
