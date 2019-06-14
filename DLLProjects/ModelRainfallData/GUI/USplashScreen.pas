unit USplashScreen;
                                                     
interface

uses
  // Delphi
  Classes, Forms, StdCtrls, ExtCtrls, SysUtils;

type
  TSplashScreen = class(TObject)
  private
    FForm : TForm;
    FOkButton : TButton;
    FImage : TImage;
    FMemo : TMemo;
  protected
    procedure CreateMemberObjects;
    procedure InitialiseMemberObjects;
    procedure DestroyMemberObjects;
    procedure ButtonOnClick(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ShowFormModal;
    procedure SplashScreenMode;
    procedure HideForm;
    procedure AddProgressString(AString : string);
  end;

implementation

uses
  // Delphi
  Graphics,
  // DWAF
  UErrorHandlingOperations, Controls;

{ TSplashScreen }

procedure TSplashScreen.AddProgressString(AString: string);
const OPNAME = 'TSplashScreen.AddProgressString';
begin
  try
    if Assigned(FMemo) then
    begin
      FMemo.Lines.Add(AString);
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TSplashScreen.ButtonOnClick(Sender: TObject);
const OPNAME = 'TSplashScreen.ButtonOnClick';
begin
  try
    FForm.Close;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

constructor TSplashScreen.Create;
begin
  inherited Create;
  CreateMemberObjects;
  InitialiseMemberObjects;
end;

procedure TSplashScreen.CreateMemberObjects;
const OPNAME = 'TSplashScreen.CreateMemberObjects';
begin
  try
    FForm := TForm.Create(nil);
    FOkButton := TButton.Create(FForm);
    FImage := TImage.Create(FForm);
    FMemo := TMemo.Create(FForm);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

destructor TSplashScreen.Destroy;
begin
  DestroyMemberObjects;
  inherited Destroy;
end;

procedure TSplashScreen.DestroyMemberObjects;
const OPNAME = 'TSplashScreen.DestroyMemberObjects';
begin
  try
    // FForm.Release;
    FreeAndNil(FForm);
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TSplashScreen.HideForm;
const OPNAME = 'TSplashScreen.HideForm';
begin
  try
    if Assigned(FForm) then
      FForm.Hide;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TSplashScreen.InitialiseMemberObjects;
const OPNAME = 'TSplashScreen.InitialiseMemberObjects';
begin
  try
    if Assigned(FForm) then
    begin
      FForm.Caption := Application.Title;
      FOkButton.Parent := FForm;
      FImage.Parent := FForm;
      FMemo.Parent := FForm;

      FMemo.Lines.Clear;
      FMemo.Readonly := true;

      FMemo.Font.Size := 10;
      FMemo.Font.Style := [fsBold];
      FMemo.Alignment := taCenter;

      FMemo.ScrollBars := ssVertical;

      FForm.BorderStyle := bsDialog;
      FForm.Position := poDesktopCenter;

      FImage.Picture.Bitmap.LoadFromResourceName(HInstance, 'SPLASHSCREEN');

      FImage.Top := 0;
      FImage.Height := FImage.Picture.Bitmap.Height;
      FImage.Width := FImage.Picture.Bitmap.Width;

      FMemo.Height := 60;
      FMemo.Top := FImage.Height + 1;

      FForm.ClientWidth  := FImage.Picture.Bitmap.Width;
      FForm.ClientHeight := FImage.Picture.Bitmap.Height + FMemo.Height + 1;

      FImage.Align := alTop;
      FMemo.Align := alClient;

      // FForm.Caption := 'DWAF Rainfall Gauge Selector Application V1.0';
      FOkButton.Caption := FAppModules.Language.GetString('ButtonCaption.Ok/Close');

      FOkButton.OnClick := ButtonOnClick;

      FOkButton.Top := FForm.ClientHeight - FOkButton.Height - 5;
      FOkButton.Left := FForm.ClientWidth div 2 - FOkButton.Width div 2;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TSplashScreen.ShowFormModal;
const OPNAME = 'TSplashScreen.ShowFormModal';
begin
  try
    if Assigned(FForm) then
    begin
      FOkButton.Visible := true;
      FMemo.Visible := False;
      FForm.ClientHeight := FImage.Height;
      FForm.Width := FImage.Width;
      FOkButton.Top := FForm.ClientHeight - FOkButton.Height - 5;
      FOkButton.Left := FForm.ClientWidth div 2 - FOkButton.Width div 2;
      FForm.ShowModal;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TSplashScreen.SplashScreenMode;
const OPNAME = 'TSplashScreen.SplashScreenMode';
begin
  try
    if Assigned(FForm) then
    begin
      FOkButton.Visible := false;
      FForm.Show;
      FForm.Update;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

end.
