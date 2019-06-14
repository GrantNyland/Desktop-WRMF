unit UThreadProgress;

interface

uses
  Classes;

type
  TProgress = class(TThread)
  private
    procedure Increment;
    procedure TerminationCode(Sender : TObject);
  protected
    procedure Execute; override;
  end;

implementation

uses
  SysUtils,
  UGenerateForm,
  UErrorHandlingOperations;

{ TProgress }

procedure TProgress.Increment;
const OPNAME = 'TProgress.Increment';
begin
  try
    with fmGenerate.prgFlowGenerate do
    begin
      if Position <> Max then
        StepIt
      else
        Position := Min;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TProgress.TerminationCode(Sender : TObject);
const OPNAME = 'TProgress.TerminationCode';
begin
  try
    fmGenerate.prgFlowGenerate.Position := fmGenerate.prgFlowGenerate.Min;
    //fmVerify.prgFlowGenerate.Hide;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TProgress.Execute;
const OPNAME = 'TProgress.Execute';
begin
  try
    //make use of a timer to set the steps to reasonable amounts
    Self.OnTerminate := TerminationCode;
    //fmVerify.prgFlowGenerate.Show;
    while NOT(Terminated) do
      Synchronize(Increment);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
