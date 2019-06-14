//
//
//  UNIT      : Contains UWaitScreen utility.
//  AUTHOR    : Grant Nyland
//  DATE      : 2002/04/18
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UWaitScreen;

interface

procedure LaunchWaitScreen(ACaption, ALabel: string);
procedure CloseWaitScreen;

implementation

uses
  SysUtils,
  Windows,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  UErrorHandlingOperations;

var
  GWaitScreen: TForm;

procedure LaunchWaitScreen(ACaption, ALabel: string);
const OPNAME = 'UWaitScreen.LaunchWaitScreen';
var LPanel: TPanel;
begin
  try

    // Make sure this is not a duplicate.
    CloseWaitScreen;

    // Create the form.
    GWaitScreen := TForm.Create(nil);

    // Set form properties.
    GWaitScreen.Cursor      := crHourGlass;
    GWaitScreen.BorderIcons := [];
    GWaitScreen.BorderStyle := bsDialog;
    GWaitScreen.Caption     := ACaption;
    GWaitScreen.Position    := poScreenCenter;
    GWaitScreen.Height      := 60;
    GWaitScreen.Width       := GWaitScreen.Canvas.TextWidth(ALabel) + 40;

    // Load the splash image.
    LPanel := TPanel.Create(GWaitScreen);
    LPanel.Parent  := GWaitScreen;
    LPanel.Align   := alClient;
    LPanel.Caption := ALabel;

    // Show the splash screen.
    GWaitScreen.Show;
    GWaitScreen.Update;
    Application.ProcessMessages;

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure CloseWaitScreen;
const OPNAME = 'UWaitScreen.CloseWaitScreen';
begin
  try
    if Assigned(GWaitScreen) then
    begin
      GWaitScreen.Close;
      FreeAndNil(GWaitScreen);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
