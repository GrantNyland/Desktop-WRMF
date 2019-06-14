unit UErrorForm;

{This form is used to report any errors that occur}


interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.StdCtrls, VCL.ExtCtrls;

type
  TfmErrorReporting = class(TForm)
    Panel1: TPanel;
    btnClose: TButton;
    scbErrorReport: TScrollBox;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmErrorReporting: TfmErrorReporting;

implementation

uses
  UDataModule,
  UErrorHandlingOperations;


{$R *.DFM}

procedure TfmErrorReporting.btnCloseClick(Sender: TObject);
const OPNAME = 'TfmErrorReporting.btnCloseClick';
begin
  try
    Close;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfmErrorReporting.FormCreate(Sender: TObject);
const OPNAME = 'TfmErrorReporting.FormCreate';
var
  NewBevel : TBevel;
  NewLabel : TLabel;
  Loop : Integer;
  ColourValue : byte;
begin
  try
    ColourValue := 1;
    for Loop := 1 to fmData.DataStorage.ErrorList.Count do
    begin
      NewLabel := TLabel.Create(scbErrorReport);
      NewLabel.Align := alTop;
      if ColourValue = 1 then
      begin
        NewLabel.Color := clBtnFace;
        ColourValue := 0;
      end
      else
      begin
        NewLabel.Color := clWindow;
        ColourValue := 1;
      end;
      NewLabel.Caption := fmData.DataStorage.ErrorList.Strings[Loop-1];
      NewLabel.Parent := scbErrorReport;

      NewBevel := TBevel.Create(scbErrorReport);
      NewBevel.Align := alTop;
      NewBevel.Shape := bsTopLine;
      NewBevel.Height := 4;
      NewBevel.Parent := scbErrorReport;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
