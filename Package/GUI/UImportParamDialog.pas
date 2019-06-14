unit UImportParamDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls;

type
  TfrmImportParam = class(TForm)
    Panel1: TPanel;
    btnOK: TButton;
    chkboxImportHyrology: TCheckBox;
    lblMessage: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImportParam: TfrmImportParam;

implementation

{$R *.dfm}

end.
