program TestCurve;

uses
  Forms,
  TestCurveMain in 'TestCurveMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
