unit UWRMFCheckBox;

interface

uses
  SysUtils, Classes, VCL.Controls, VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics,

  UWRMFControl;

type
  TWRMFCheckBox = class(TWRMFControl)
  protected
    { Protected declarations }
    FCheckBox : TCheckBox;
    procedure Resize; override;
    procedure ChangeColor; override;
    function GetCaption : String;
    procedure SetCaption (ACaption : String);
    function GetAlignment : TLeftRight;
    procedure SetAlignment (AAlign : TLeftRight);
    function GetChecked : Boolean;
    procedure SetChecked (AChecked : Boolean);
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
    property Checked    : Boolean     read GetChecked    write SetChecked;
    property Caption    : String      read GetCaption    write SetCaption;
    property Alignment  : TLeftRight  read GetAlignment  write SetAlignment;
  end;

procedure Register;

implementation

uses
  UErrorHandlingOperations;

procedure Register;
begin
  RegisterComponents('WRMF', [TWRMFCheckBox]);
end;

constructor TWRMFCheckBox.Create(AOwner: TComponent);
const OPNAME = 'TWRMFCheckBox.Create';
begin
  try
    inherited Create(AOwner);

    // Create the check box.
    FCheckBox := TCheckBox.Create(self);
    FCheckBox.Parent := self;
    FCheckBox.Top    := 0;
    FCheckBox.Left   := 0;
    FCheckBox.Height := 21;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFCheckBox.Resize;
const OPNAME = 'TWRMFCheckBox.Resize';
begin
  try
    inherited Resize;
    ClientHeight   := FCheckBox.Height;
    FCheckBox.Width := ClientWidth - 10;
    FParamChangeIndicator.Left := ClientWidth - 10;
    FMetaDataIndicator.Left    := ClientWidth - 10;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFCheckBox.SetFocus;
const OPNAME = 'TWRMFCheckBox.SetFocus';
begin
  try
    FCheckBox.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFCheckBox.GetCaption : String;
const OPNAME = 'TWRMFCheckBox.GetCaption';
begin
  try
    Result := FCheckBox.Caption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFCheckBox.SetCaption (ACaption : String);
const OPNAME = 'TWRMFCheckBox.SetCaption';
begin
  try
    FCheckBox.Caption := ACaption;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFCheckBox.GetAlignment : TLeftRight;
const OPNAME = 'TWRMFCheckBox.GetAlignment';
begin
  Result := taLeftJustify;
  try
    Result := FCheckBox.Alignment;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFCheckBox.SetAlignment (AAlign : TLeftRight);
const OPNAME = 'TWRMFCheckBox.SetAlignment';
begin
  try
    FCheckBox.Alignment := AAlign;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFCheckBox.GetChecked : Boolean;
const OPNAME = 'TWRMFCheckBox.GetChecked';
begin
  Result := FALSE;
  try
    Result := FCheckBox.Checked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFCheckBox.SetChecked (AChecked : Boolean);
const OPNAME = 'TWRMFCheckBox.SetChecked';
begin
  try
    FCheckBox.Checked := AChecked;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFCheckBox.GetColor : TColor;
const OPNAME = 'TWRMFCheckBox.GetColor';
begin
  Result := 0;
  try
    Result := FCheckBox.Color;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFCheckBox.SetColor(AColor: TColor);
const OPNAME = 'TWRMFCheckBox.SetColor';
begin
  try
    FCheckBox.Color := AColor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFCheckBox.SetActive(AActive: Boolean);
const OPNAME = 'TWRMFCheckBox.SetActive';
begin
  try
    FCheckBox.Enabled := AActive;
    inherited SetActive(AActive);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFCheckBox.ChangeColor;
const OPNAME = 'TWRMFCheckBox.ChangeColor';
begin
  try
    if (NOT FIsValid) then
      FCheckBox.Color := clRed
    else
      FCheckBox.Color := clBtnFace;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFCheckBox.GetOnChange: TNotifyEvent;
const OPNAME = 'TWRMFCheckBox.GetOnChange';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFCheckBox.SetOnChange(AOnChange: TNotifyEvent);
const OPNAME = 'TWRMFCheckBox.SetOnChange';
begin
end;

function TWRMFCheckBox.GetOnExit: TNotifyEvent;
const OPNAME = 'TWRMFCheckBox.GetOnExit';
begin
  Result := FCheckBox.OnExit;
end;

procedure TWRMFCheckBox.SetOnExit(AOnExit: TNotifyEvent);
const OPNAME = 'TWRMFCheckBox.SetOnExit';
begin
  FCheckBox.OnExit := AOnExit;
end;

end.
