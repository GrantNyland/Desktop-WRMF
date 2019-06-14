unit UWRMFEdit;

interface

uses
  SysUtils, Classes, VCL.Controls, VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics,

  UWRMFControl;

type
  TWRMFEdit = class(TWRMFControl)
  protected
    { Protected declarations }
    FEditBox : TEdit;
    procedure Resize; override;
    procedure ChangeColor; override;
    function GetText : string; override;
    procedure SetText (AText : string); override;
    function GetColor : TColor; override;
    procedure SetColor (AColor : TColor); override;
    procedure SetActive (AActive : Boolean); override;
    function GetOnChange: TNotifyEvent; override;
    procedure SetOnChange(AOnChange: TNotifyEvent); override;
    function GetOnExit: TNotifyEvent; override;
    procedure SetOnExit(AOnExit: TNotifyEvent); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
  UErrorHandlingOperations;

procedure Register;
begin
  RegisterComponents('WRMF', [TWRMFEdit]);
end;

constructor TWRMFEdit.Create(AOwner: TComponent);
const OPNAME = 'TWRMFEdit.Create';
begin
  try
    inherited Create(AOwner);

    // Create the edit box.
    FEditBox := TEdit.Create(self);
    FEditBox.Parent := self;
    FEditBox.Top    := 0;
    FEditBox.Left   := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFEdit.Resize;
const OPNAME = 'TWRMFEdit.Resize';
begin
  try
    inherited Resize;
    ClientHeight   := FEditBox.Height;
    FEditBox.Width := ClientWidth - 10;
    FParamChangeIndicator.Left := ClientWidth - 10;
    FMetaDataIndicator.Left    := ClientWidth - 10;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFEdit.SetFocus;
const OPNAME = 'TWRMFEdit.SetFocus';
begin
  try
    FEditBox.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFEdit.GetText : String;
const OPNAME = 'TWRMFEdit.GetText';
begin
  try
    Result := FEditBox.Text;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFEdit.SetText(AText: string);
const OPNAME = 'TWRMFEdit.SetText';
begin
  try
    FEditBox.Text := AText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFEdit.GetColor : TColor;
const OPNAME = 'TWRMFEdit.GetColor';
begin
  Result := 0;
  try
    Result := FEditBox.Color;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFEdit.SetColor(AColor: TColor);
const OPNAME = 'TWRMFEdit.SetColor';
begin
  try
    FEditBox.Color := AColor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFEdit.SetActive(AActive: Boolean);
const OPNAME = 'TWRMFEdit.SetActive';
begin
  try
    FEditBox.Enabled := AActive;
    inherited SetActive(AActive);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFEdit.ChangeColor;
const OPNAME = 'TWRMFEdit.ChangeColor';
begin
  try
    if (NOT FIsValid) then
      FEditBox.Color := clRed
    else if (NOT FEditBox.Enabled) then
      FEditBox.Color := clSilver
    else
      FEditBox.Color := clWindow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFEdit.GetOnChange: TNotifyEvent;
const OPNAME = 'TWRMFEdit.GetOnChange';
begin
  try
    Result := FEditBox.OnChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFEdit.SetOnChange(AOnChange: TNotifyEvent);
const OPNAME = 'TWRMFEdit.SetOnChange';
begin
  try
    FEditBox.OnChange := AOnChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFEdit.GetOnExit: TNotifyEvent;
const OPNAME = 'TWRMFEdit.GetOnExit';
begin
  try
    Result := FEditBox.OnExit;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFEdit.SetOnExit(AOnExit: TNotifyEvent);
const OPNAME = 'TWRMFEdit.SetOnExit';
begin
  try
    FEditBox.OnExit := AOnExit;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
