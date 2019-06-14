unit UReservoirPenaltyActionDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.CheckLst, VCL.ExtCtrls,

  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent;

type
  TPenaltyAction = (paNone,paAdd,paDelete);
  TfrmPenaltyAction = class(TForm)
    btnOk           : TButton;
    btnCancel       : TButton;
    lblCopy         : TLabel;
    lblInsert       : TLabel;
    lblDelete       : TLabel;
    CmbCopy         :TComboBox;
    CmbInsert       : TComboBox;
    CmbDelete       : TComboBox;
    procedure FormCreate(Sender: TObject);
 //   procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FPenaltyAction:TPenaltyAction;
  public
    { Public declarations }
    procedure SetPenaltyAction(AAction:TPenaltyAction);
  end;

var
  frmPenaltyAction: TfrmPenaltyAction;

implementation

{$R *.dfm}

procedure TfrmPenaltyAction.FormCreate(Sender: TObject);
const OPNAME = 'TfrmPenaltyAction.FormCreate';
begin
  FPenaltyAction := paNone;
end;

procedure TfrmPenaltyAction.SetPenaltyAction(AAction: TPenaltyAction);
const OPNAME = 'TfrmPenaltyAction.SetPenaltyAction';
begin
  FPenaltyAction := AAction;
  case FPenaltyAction of
    paNone:
      begin
        CmbCopy.Visible   := False;
        CmbInsert.Visible := False;
        CmbDelete.Visible := False;
        btnOk.Enabled        := False;
        btnCancel.Enabled    := True;
      end;
    paAdd:
      begin
        CmbCopy.Visible   := True;
        lblCopy.Visible   := True;
        CmbInsert.Visible := True;
        lblInsert.Visible := True;
        CmbDelete.Visible := False;
        lblDelete.Visible := False;
        btnOk.Enabled     := True;
        btnOk.Top         := 80;
        btnCancel.Enabled := True;
        btnCancel.Top     := 80;
        Self.ClientHeight := 150;
      end;
    paDelete:
      begin
        CmbCopy.Visible   := False;
        lblCopy.Visible   := False;
        CmbInsert.Visible := False;
        lblInsert.Visible := False;
        CmbDelete.Visible := True;
        lblDelete.Visible := True;
        btnOk.Enabled     := True;
        btnOk.Top         := 48;
        btnCancel.Enabled := True;
        btnCancel.Top     := 48;
        CmbDelete.Top     := 16;
        lblDelete.Top     := CmbDelete.Top;
        Self.ClientHeight := 140;
      end;
  end;
  //FormResize(nil);
end;

{procedure TfrmPenaltyAction.FormResize(Sender: TObject);
begin
  case FPenaltyAction of
    paAdd:
      begin
        btnOk.Left           := 45;
        btnCancel.Left       := 184;
        CmbDelete.Left        := 0;
      end;
    paDelete:
      begin
        btnOk.Left           := 13;
        btnCancel.Left       := 120;
        CmbDelete.Left        := 45;
      end;
  end;
end;}

end.
