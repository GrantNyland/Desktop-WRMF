//
//
//  UNIT      : Contains the class TAccessControlDialog.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/02/05
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAccessControlDialog;


interface


{$R *.DFM}


//
// Interface dependencies
//
uses

  // DWAF
  UUser,
  UAbstractObject,
  UAbstractComponent,

  // Delphi
  Forms,
  Classes,
  Controls,
  ComCtrls,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  Windows,
  DB;

//
// This dialog is used to select a study area.
//
type
  TAccessControlDialog = class(TAbstractForm)
    FSplitter: TSplitter;
    FOKButton: TBitBtn;
    FCancelButton: TBitBtn;
    FUsersDetailsPanel: TPanel;
    FPanelTop: TPanel;
    FLabelUserID: TLabel;
    FLabelPassword: TLabel;
    FEditUserID: TEdit;
    FEditPassword: TEdit;
    procedure FEditPasswordEnter(Sender: TObject);
  public
  end;


implementation


//
// Implementation dependencies
//
uses

  // Delphi
  SysUtils,
  Graphics,
  
  // DWAF
  UErrorHandlingOperations;

procedure TAccessControlDialog.FEditPasswordEnter(Sender: TObject);
const OPNAME = 'TAccessControlDialog.FEditPasswordEnter';
begin
  FEditPassword.Color := clWindow;
end;

end.

