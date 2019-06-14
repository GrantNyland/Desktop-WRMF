unit URenameDailyDiversionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls,
  UAbstractComponent;

type
  TfrmRenameDailyDiversion = class(TAbstractForm)
    lblTop: TLabel;
    edtTop: TEdit;
    lblBottom: TLabel;
    edtBottom: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure EnableAllControls;
  public
    { Public declarations }
  end;

var
  frmRenameDailyDiversion: TfrmRenameDailyDiversion;

implementation

{$R *.dfm}

procedure TfrmRenameDailyDiversion.FormShow(Sender: TObject);
const OPNAME = 'TfrmRenameDailyDiversion.FormShow';
begin
  Self.ClientHeight := 102;
  Self.ClientWidth  := 450;
  EnableAllControls;
  lblTop.Enabled    := False;
  edtTop.Enabled    := False;
end;

procedure TfrmRenameDailyDiversion.EnableAllControls;
const OPNAME = 'TfrmRenameDailyDiversion.EnableAllControls';
begin
  lblTop.Visible    := True;
  lblTop.Enabled    := True;
  edtTop.Visible    := True;
  edtTop.Enabled    := True;
  lblBottom.Visible := True;
  lblBottom.Enabled := True;
  edtBottom.Visible := True;
  edtBottom.Enabled := True;
end;

end.
