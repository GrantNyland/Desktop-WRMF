//
//
//  UNIT      : Contains the class TProgressDialog.
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/03/05
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit URunOptionsDialog;


interface


{$R *.DFM}


//
// Interface dependencies
//
uses
  // DWAF
  UAbstractComponent,

  // Delphi
  VCL.Forms,
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  Contnrs,
  VCL.Dialogs,
  VCL.CheckLst;

//
// This dialog is used to select a study area.
//
type
  TRunOptionsDialog = class(TAbstractForm)
    FButtonPanel: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    gbxRunOptions: TGroupBox;
    gbxOutputOptions: TGroupBox;
    ChklBoxRunOptions: TCheckListBox;
    ChklBoxOutputOptions: TCheckListBox;
    splRunOutputOptions: TSplitter;
    procedure OnChklBoxRunOptionsClickCheck(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
  public
  end;


implementation
uses
  UErrorHandlingOperations,
  SysUtils;

procedure TRunOptionsDialog.OnChklBoxRunOptionsClickCheck(Sender: TObject);
const OPNAME = 'TRunOptionsDialog.OnChklBoxRunOptionsClickCheck';
begin
  try
    ChklBoxOutputOptions.Enabled := not ChklBoxRunOptions.Checked[4];
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TRunOptionsDialog.FormCreate(Sender: TObject);
const OPNAME = 'TRunOptionsDialog.FormCreate';
begin
  try
    ChklBoxRunOptions.OnClickCheck := OnChklBoxRunOptionsClickCheck;
  except on E: Exception do HandleError(E, OPNAME) end;    
end;

end.
