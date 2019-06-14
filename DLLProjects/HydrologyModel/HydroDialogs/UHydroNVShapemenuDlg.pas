unit UHydroNVShapeMenuDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.Menus, VCL.ComCtrls;

const
  clVNVBlueGray  = $00CDB6AF;
  CVNVItemHeight = 22;
type
  THydroNVShapeMenuDlg = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FOption       : integer;
    FOptionPanels : TList;
    FOptionList   : TStringList;
  protected
    procedure DoMouseMove (ASender : TObject;
                           AShift  : TShiftState;
                           AXPos   : integer;
                           AYPos   : Integer);
    procedure DoOptionClicked (ASender : TObject);
  public
    function ShowShapeMenuDlg : integer;
    property Options : TStringList read FOptionList;
  end;

implementation

uses
  VCL.ExtCtrls,
  UErrorHandlingOperations;

{$R *.dfm}

procedure THydroNVShapeMenuDlg.FormCreate(Sender: TObject);
const OPNAME = 'THydroNVShapeMenuDlg.FormCreate';
begin
	try
    FOption         := -1;
    FOptionPanels   := TList.Create;
    FOptionList     := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVShapeMenuDlg.FormDestroy(Sender: TObject);
const OPNAME = 'THydroNVShapeMenuDlg.FormDestroy';
begin
  try
    FreeAndNil(FOptionPanels);
    FreeAndNil(FOptionList);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function THydroNVShapeMenuDlg.ShowShapeMenuDlg : integer;
const OPNAME = 'THydroNVShapeMenuDlg.ShowShapeMenuDlg';
var
  LIndex    : integer;
  LPanel    : TPanel;
  LWidth    : integer;
  LMaxWidth : integer;
begin
  Result := -1;
	try
    Self.Left := Mouse.CursorPos.X;
    Self.Top  := Mouse.CursorPos.Y;
    lMaxWidth := 0;
    for LIndex := 0 to FOptionList.Count - 1 do
    begin
      LPanel := TPanel.Create(Self);
      with LPanel do
      begin
        Parent      := Self;
        Color       := clWhite;
        Left        := 0;
        Top         := LIndex * CVNVItemHeight;
        Height      := CVNVItemHeight;
        Align       := alTop;
        Alignment   := taLeftJustify;
        Caption     := '   ' + FOptionList.Names[LIndex];
        TabOrder    := LIndex;
        OnMouseMove := DoMouseMove;
        OnClick     := DoOptionClicked;
      end;
      FOptionPanels.Add(LPanel);
      LWidth := Self.Canvas.TextWidth(LPanel.Caption);
      if (LWidth > lMaxWidth) then
        lMaxWidth := LWidth;
    end;
    Self.ClientHeight := CVNVItemHeight * FOptionList.Count;
    Self.ClientWidth  := lMaxWidth + 10;
    if (Self.Left + Self.Width > Screen.Width) then
      Self.Left := Screen.Width - Self.Width - 15;
    if (Self.Top + Self.Height > Screen.Height) then
      Self.Top := Screen.Height - Self.Height - 15;
    ShowModal;
    Result := FOption;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVShapeMenuDlg.DoMouseMove (ASender : TObject;
                                       AShift  : TShiftState;
                                       AXPos   : integer;
                                       AYPos   : Integer);
const OPNAME = 'THydroNVShapeMenuDlg.DoMouseMove';
var
  LIndex       : integer;
  LPanel       : TPanel;
  lSenderPanel : TPanel;
begin
  try
    lSenderPanel := ASender as TPanel;
    for LIndex := 0 to FOptionPanels.Count - 1 do
    begin
      LPanel := TPanel(FOptionPanels.Items[LIndex]);
      if (LPanel = lSenderPanel) then
        LPanel.Color := clVNVBlueGray
      else
        LPanel.Color := clWhite;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure THydroNVShapeMenuDlg.DoOptionClicked (ASender : TObject);
const OPNAME = 'THydroNVShapeMenuDlg.DoOptionClicked';
var
  LIndex       : integer;
  LPanel       : TPanel;
  lSenderPanel : TPanel;
  LMenuDlg     : THydroNVShapeMenuDlg;
  LCount       : Integer;
  LTempList    : TStringList;
begin
  try
    FOption := -1;
    lSenderPanel := ASender as TPanel;
    LIndex := 0;
    while ((FOption = -1) AND (LIndex < FOptionPanels.Count)) do
    begin
      LPanel := TPanel(FOptionPanels.Items[LIndex]);
      if (LPanel = lSenderPanel) then
      begin
        if (FOptionList.Objects[LIndex] <> nil) then
        begin
          LTempList := TStringList(FOptionList.Objects[LIndex]);
          LMenuDlg := THydroNVShapeMenuDlg.Create(nil);
          for LCount := 0 to LTempList.Count - 1 do
            LMenuDlg.Options.AddObject(LTempList.Strings[LCount], LTempList.Objects[LCount]);
          try
            FOption := LMenuDlg.ShowShapeMenuDlg;
            if (FOption <> 0) then
              ModalResult := mrOk;
          finally
            FreeAndNil(LMenuDlg);
          end;
        end
        else
        begin
          FOption := StrToInt(FOptionList.ValueFromIndex[LIndex]);
          ModalResult := mrOk;
        end;
      end
      else
        LIndex := LIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
