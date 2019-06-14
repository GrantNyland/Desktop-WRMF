unit UGISForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.Menus, VCL.ExtCtrls, VCL.StdCtrls, VCL.ComCtrls;

type
  TGISForm = class(TForm)
    FPnlButtons : TPanel;
    FBtnOK      : TButton;
    FBtnCancel  : TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GISForm: TGISForm;

implementation

uses
  ShellApi,
  IniFiles,
  Registry,

  UErrorHandlingOperations;

{$R *.dfm}


end.
