unit UHydroNVProgressDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.Menus, VCL.ComCtrls;

type
  THydroNVProgressDlg = class(TForm)
    FProgressBar       : TProgressBar;
    FNrOfShapesCaption : TLabel;
    FNrOfShapesValue   : TLabel;
    FOfTotalCaption    : TLabel;
  private
  public
	  procedure UpdateProgress (ACount : integer);
	  procedure SetMaximum (ATotal : integer);
  end;

implementation

uses

  UErrorHandlingOperations;

{$R *.dfm}

procedure THydroNVProgressDlg.UpdateProgress (ACount : integer);
const OPNAME = 'THydroNVProgressDlg.UpdateProgress';
begin
	try
    FProgressBar.Position := ACount;
    FNrOfShapesValue.Caption := IntToStr(ACount);
    Self.Repaint;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVProgressDlg.SetMaximum (ATotal : integer);
const OPNAME = 'THydroNVProgressDlg.SetMaximum';
begin
	try
    FProgressBar.Max := ATotal;
    FOfTotalCaption.Caption := ' of ' + IntToStr(ATotal);
    Height := 100;
  except on E: Exception do HandleError(E, OPNAME); end;
end;


end.
