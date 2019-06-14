unit UMessagesForm;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.StdCtrls;

type
  TfmMessages = class(TForm)
    lblMessage: TLabel;
  private
    { Private declarations }
  public
    procedure DisplayMessage(TheMessage  : string);
    { Public declarations }
    function ResetState: boolean; virtual;
    function Initialise: boolean; virtual;
    function LanguageHasChanged: boolean; virtual;
    function StudyHasChanged: boolean; virtual;
  end;

var
  fmMessages: TfmMessages;

implementation

uses
  UErrorHandlingOperations;


{$R *.DFM}

procedure TfmMessages.DisplayMessage(TheMessage  : string);
const OPNAME = 'TfmMessages.DisplayMessage';
begin
  try
    //self.hide;
    lblMessage.Caption := TheMessage;
    lblMessage.Left := 0;
    lblMessage.Top := 0;
    //self.ClientWidth := lblMessage.Width;
    //self.ClientHeight := lblMessage.Height;
    self.Show;
    self.Refresh;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TfmMessages.Initialise: boolean;
const OPNAME = 'TfmMessages.Initialise';
begin
  Result := False;
  try
    lblMessage.Caption := '';
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmMessages.LanguageHasChanged: boolean;
const OPNAME = 'TfmMessages.LanguageHasChanged';
begin
  Result := False;
  try
    lblMessage.Caption := '';
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmMessages.ResetState: boolean;
const OPNAME = 'TfmMessages.ResetState';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TfmMessages.StudyHasChanged: boolean;
const OPNAME = 'TfmMessages.StudyHasChanged';
begin
  Result := False;
  try
    lblMessage.Caption := '';
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
