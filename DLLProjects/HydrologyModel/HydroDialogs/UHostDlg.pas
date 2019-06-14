unit UHostDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,
  VCL.ComCtrls;

type
  THostDlg = class(TForm)
    PgcHost : TPageControl;
  private
  public
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}


end.
