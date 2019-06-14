unit UTimeSeriesComparitorCaptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids, StdCtrls, Buttons;

type
  TfrmFormTest = class(TForm)
    strgrdCaptions: TStringGrid;
    pnlButtons: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFormTest: TfrmFormTest;

implementation

{$R *.dfm}

end.
