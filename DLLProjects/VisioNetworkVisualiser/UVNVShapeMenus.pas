{******************************************************************************}
{*  UNIT      : Contains the classes VNVShape menus.                          *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/10/21                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UVNVShapeMenus;

interface

uses
  Classes,
  VCL.Menus;
type
  TVNVShapeMenuDlg = class(TPopupMenu)
  protected
    FIndex : integer;
    procedure OnMenuItemClick(Sender : TObject);
  public
	  function ShowShapeMenuDialog (AOptionList : TStringList): integer;
  end;

implementation
uses
  VCL.Forms,
	VCL.Controls,
  SysUtils,
  VCL.dialogs,
  UErrorHandlingOperations;

{******************************************************************************}
{* TVNVShapeMenuDlg                                                       *}
{******************************************************************************}

procedure TVNVShapeMenuDlg.OnMenuItemClick(Sender: TObject);
const OPNAME = 'TVNVShapeMenuDlg.ShowShapeMenuDialog';
var
  lMenuItem : TMenuItem;
begin
  try
    lMenuItem := TMenuItem(Sender);
    FIndex    := Self.Items.IndexOf(lMenuItem);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TVNVShapeMenuDlg.ShowShapeMenuDialog (AOptionList : TStringList): integer;
const OPNAME = 'TVNVShapeMenuDlg.ShowShapeMenuDialog';
var
  lIndex    : integer;
  lMenuItem : TMenuItem;
begin
  Result := -1;
	try
    FIndex := -1;
    for lIndex := 0 to AOptionList.Count - 1 do
    begin
      lMenuItem := TMenuItem.Create(Self);
      lMenuItem.Caption     := '   ' + AOptionList.Strings[lIndex];
      lMenuItem.OnClick     := OnMenuItemClick;
      Self.Items.Add(lMenuItem);
      lMenuItem.Enabled := (integer(AOptionList.Objects[lIndex]) <> 0);
    end;
    Self.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
    Application.ProcessMessages;
    Result := FIndex;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.


