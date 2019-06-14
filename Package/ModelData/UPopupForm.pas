unit UPopupForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Contnrs,
  System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.UITypes,
  System.Actions, Vcl.Menus, Winapi.Imm, Vcl.ActnList,
  Winapi.MultiMon, System.HelpIntfs, Winapi.DwmApi, Vcl.Themes, System.Win.TaskbarCore,
  Winapi.ShellScaling,

  UAbstractObject,
  UDataComponent,
  UAbstractComponent;


type
  TfrmPopupForm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    pnlButton: TPanel;

  private
 
  public
    function Initialise: boolean;
   end;

var
  frmPopupForm: TfrmPopupForm;



implementation
  Uses
    UErrorHandlingOperations;


{$R *.dfm}


function TfrmPopupForm.Initialise: boolean;
const OPNAME = 'TfrmPopupDialog.Initialise';
begin
  Result := False;
  try
    Self.Position := poScreenCenter;
    Self.FormStyle := fsNormal;
    Self.Height := 548;
    Self.Width := 848;
    BorderStyle := bsSizeToolWin;
    Caption := Application.Title;

    BorderStyle := bsSizeToolWin;

    Caption := Application.Title;

   
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;






end.
