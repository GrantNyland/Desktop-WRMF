unit USplashForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, jpeg;

type
  TfmSplash = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    Label6: TLabel;
    Label7: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    imgBack: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure AnyWhereClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmSplash: TfmSplash;

implementation

uses
  UErrorHandlingOperations;

var
  Counter : Integer;
{$R *.DFM}


procedure TfmSplash.FormCreate(Sender: TObject);
const OPNAME = 'TfmSplash.FormCreate';
begin
  try
    Counter:=0;
    Timer1.Enabled := true;//start timer to destroy form
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSplash.Timer1Timer(Sender: TObject);
const OPNAME = 'TfmSplash.Timer1Timer';
begin
  try
    //when time complete free the form resources
    Inc(Counter);
    If Counter>4 Then Begin
      Timer1.Enabled:=False;
      fmSplash.Close;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSplash.AnyWhereClick(Sender: TObject);
const OPNAME = 'TfmSplash.AnyWhereClick';
begin
  try
    Timer1.Enabled:=False;
    fmSplash.Close;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmSplash.FormClose(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'TfmSplash.FormClose';
begin
  try
    Application.MainForm.Show;//Show the main form
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
