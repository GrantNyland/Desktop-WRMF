unit UTimeSeriesComparitorChartActionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls,
  UAbstractComponent;

type
  TChartAction = (cacNone,cacCreateChart,cacRenameChart,cacDeleteChart,cacCreateView,cacRenameView,cacDeleteView);
  TfrmChartAction = class(TAbstractForm)
    lblTop: TLabel;
    edtTop: TEdit;
    lblBottom: TLabel;
    edtBottom: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure edtTopChange(Sender: TObject);
  private
    { Private declarations }
    FOldText: string;
    FChartAction:TChartAction;
    procedure EnableAllControls;
  public
    { Public declarations }
    property Action: TChartAction read FChartAction write FChartAction;
  end;

var
  frmChartAction: TfrmChartAction;

implementation

{$R *.dfm}

procedure TfrmChartAction.FormShow(Sender: TObject);
const OPNAME = 'TfrmChartAction.FormShow';
begin
  Self.ClientHeight := 102;
  Self.ClientWidth  := 366;

  EnableAllControls;
  case Action of
    cacCreateChart :
      begin
        Self.Caption      := FAppModules.Language.GetString('TSCChartActionForm.CreateChart_Caption');
        lblTop.Caption    := FAppModules.Language.GetString('TSCChartActionForm.ChartName');
        lblBottom.Caption := FAppModules.Language.GetString('TSCChartActionForm.ViewName');
        lblBottom.Enabled := (Trim(edtBottom.Text) = '');
        edtBottom.Enabled := (Trim(edtBottom.Text) = '');
      end;
    cacRenameChart :
      begin
        Self.Caption      := FAppModules.Language.GetString('TSCChartActionForm.RenameChart_Caption');
        lblTop.Caption    := FAppModules.Language.GetString('TSCChartActionForm.OldChartName');
        lblBottom.Caption := FAppModules.Language.GetString('TSCChartActionForm.NewChartName');
        lblTop.Enabled    := False;
        edtTop.Enabled    := False;
      end;
    cacDeleteChart :
      begin
        Self.Caption      := FAppModules.Language.GetString('TSCChartActionForm.DeleteChart_Caption');
        lblTop.Caption    := FAppModules.Language.GetString('TSCChartActionForm.ChartName');;
        lblTop.Enabled    := False;
        edtTop.Enabled    := False;
        lblBottom.Visible := False;
        edtBottom.Visible := False;
      end;
    cacCreateView :
      begin
        Self.Caption      := FAppModules.Language.GetString('TSCChartActionForm.CreateView_Caption');
        lblTop.Caption    := FAppModules.Language.GetString('TSCChartActionForm.ViewName');
        edtTop.Text       := '';
        lblBottom.Visible := False;
        edtBottom.Visible := False;
      end;
    cacRenameView :
      begin
        Self.Caption      := FAppModules.Language.GetString('TSCChartActionForm.RenameView_Caption');
        lblTop.Caption    := FAppModules.Language.GetString('TSCChartActionForm.OldViewName');
        lblBottom.Caption := FAppModules.Language.GetString('TSCChartActionForm.NewViewName');
        lblTop.Enabled    := False;
        edtTop.Enabled    := False;
      end;
    cacDeleteView :
      begin
        Self.Caption      := FAppModules.Language.GetString('TSCChartActionForm.DeleteView_Caption');
        lblTop.Caption    := FAppModules.Language.GetString('TSCChartActionForm.ViewName');
        lblTop.Enabled    := False;
        edtTop.Enabled    := False;
        lblBottom.Visible := False;
        edtBottom.Visible := False;
      end;
  end;
end;

procedure TfrmChartAction.edtTopChange(Sender: TObject);
const OPNAME = 'TfrmChartAction.edtTopChange';
begin
  if edtBottom.Enabled and
     (Action = cacCreateChart) and
     (edtBottom.Text = FOldText) then
    edtBottom.Text := edtTop.Text;
  FOldText := edtTop.Text;
end;

procedure TfrmChartAction.EnableAllControls;
const OPNAME = 'TfrmChartAction.EnableAllControls';
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
