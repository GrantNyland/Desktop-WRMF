unit UNetworkVisualiserSplashScreen;

interface

procedure LaunchSplashScreen; export; stdcall;
procedure WaitForSplashScreen; export; stdcall;

implementation

uses
  SysUtils,
  Windows,
  Controls,
  ExtCtrls,
  Forms,
  UErrorHandlingOperations;

var
  GStartTickCount: longword;
  GSplashScreen: TForm;

procedure LaunchSplashScreen;
const OPNAME = 'UNetworkVisualiserSplashScreen.LaunchSplashScreen';
var LImage: TImage;
begin

    // Create the form.
    GSplashScreen := TForm.Create(nil);

    // Start the waiting clock.
    GStartTickCount := GetTickCount;

    // Set form properties.
    GSplashScreen.Cursor      := crHourGlass;
    GSplashScreen.BorderIcons := [];
    GSplashScreen.BorderStyle := bsDialog;
    GSplashScreen.Caption     := FAppModules.Language.GetString('VNV.NetworkVisualiser');
    GSplashScreen.FormStyle   := fsStayOnTop;
    GSplashScreen.Position    := poScreenCenter;

    // Load the splash image.
    LImage := TImage.Create(GSplashScreen);
    LImage.Parent := GSplashScreen;
    LImage.Align := alClient;
    LImage.Stretch := True;
    LImage.Picture.Bitmap.LoadFromResourceName(HInstance, 'SPLASHSCREENNV');
    GSplashScreen.ClientWidth  := LImage.Picture.Bitmap.Width;
    GSplashScreen.ClientHeight := LImage.Picture.Bitmap.Height;

    // Show the splash screen.
    GSplashScreen.Show;
    GSplashScreen.Update;
    Application.ProcessMessages;

end;

procedure WaitForSplashScreen;
const OPNAME = 'UNetworkVisualiserSplashScreen.WaitForSplashScreen';
begin
    while (GetTickCount < GStartTickCount + 4000) do
      Application.ProcessMessages;
    GSplashScreen.Close;
    FreeAndNil(GSplashScreen);
end;

end.










