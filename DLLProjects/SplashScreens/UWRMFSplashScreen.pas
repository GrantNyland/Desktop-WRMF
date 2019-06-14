unit UWRMFSplashScreen;

interface

function LaunchSplashScreen:boolean; export; stdcall;
procedure WaitForSplashScreen; export; stdcall;
implementation

uses                                     
  UUtilities,
  SysUtils,
  Windows,
  VCL.Controls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Graphics,
  VCL.Forms,
  UErrorHandlingOperations;

var
  GStartTickCount: longword;
  GSplashScreen: TForm;

function LaunchSplashScreen: boolean;
const OPNAME = 'UWRMFSplashScreen.LaunchSplashScreen';
var LImage: TImage;
    LPanel: TPanel;
    LButton1,
    LButton2: TButton;
begin
  Result := False;
  try

    // Create the form.
    GSplashScreen := TForm.Create(nil);

    // Start the waiting clock.
    GStartTickCount := GetTickCount;

    // Set form properties.
    GSplashScreen.BorderIcons := [];
    GSplashScreen.BorderStyle := bsDialog;
    GSplashScreen.Caption     := GetGeneralFormCaption(nil) ;
    //GSplashScreen.FormStyle   := fsStayOnTop;
    GSplashScreen.Position    := poScreenCenter;

    LPanel        := TPanel.Create(GSplashScreen);
    LPanel.Parent := GSplashScreen;
    LPanel.Align  := alBottom;
    LPanel.Height := 30;
    LPanel.Caption := 'Disclaimer';
    LPanel.Font.Style := [fsBold];

    LButton1 := TButton.Create(LPanel);
    LButton1.Parent := LPanel;
    LButton1.Caption := 'Accept';
    LButton1.ModalResult := mrOk;

    LButton2 := TButton.Create(LPanel);
    LButton2.Parent := LPanel;
    LButton2.Caption := 'Decline';
    LButton2.ModalResult := mrCancel;

    // Load the splash image.
    LImage := TImage.Create(GSplashScreen);
    LImage.Parent := GSplashScreen;
    LImage.Align := alClient;
    LImage.Picture.Bitmap.LoadFromResourceName(HInstance, 'SPLASHSCREEN');
    GSplashScreen.ClientWidth   := LImage.Picture.Bitmap.Width+10;
    GSplashScreen.ClientHeight  := LImage.Picture.Bitmap.Height + LPanel.Height;
    GSplashScreen.ActiveControl := LButton1;

    LButton1.Top := 2;
    LButton2.Top := 2;
    LButton2.Left := LPanel.Width - LButton2.Width - 10;
    LButton1.Left := LButton2.Left - LButton1.Width - 10 ;

    // Show the splash screen.
    GSplashScreen.ShowModal;
    Result := (GSplashScreen.ModalResult = mrOk);
    if Result then
    begin
      LPanel.Visible := False;
      GSplashScreen.Cursor      := crHourGlass;
      GSplashScreen.ClientHeight  := LImage.Picture.Bitmap.Height;
      Application.ProcessMessages;
      GSplashScreen.Update;
      GSplashScreen.Show;
      GSplashScreen.Update;
      Application.ProcessMessages;
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure WaitForSplashScreen;
const OPNAME = 'UWRMFSplashScreen.WaitForSplashScreen';
begin
  try
    while (GetTickCount < GStartTickCount + 4000) do
      Application.ProcessMessages;
    GSplashScreen.Close;
    FreeAndNil(GSplashScreen);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
