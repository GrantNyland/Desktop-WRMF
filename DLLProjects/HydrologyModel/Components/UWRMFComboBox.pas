unit UWRMFComboBox;

interface

uses
  SysUtils, Classes, VCL.Controls, VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics,

  UWRMFControl;

type
  TWRMFComboBox = class(TWRMFControl)
  protected
    { Protected declarations }
    FComboBox             : TComboBox;
    procedure ChangeColor; override;
    function GetItemIndex: Integer;
    procedure SetItemIndex(AIndex: Integer);
    function GetItems: TStrings;
    function GetText: string; override;
    procedure SetText(AText: string); override;
    function GetColor: TColor; override;
    procedure SetColor(AColor: TColor); override;
    procedure SetActive(AActive: Boolean); override;
    function GetOnChange: TNotifyEvent; override;
    procedure SetOnChange(AOnChange: TNotifyEvent); override;
    function GetOnExit: TNotifyEvent; override;
    procedure SetOnExit(AOnChange: TNotifyEvent); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
    procedure SetFocus; override;
  published
    { Published declarations }
    property Items      : TStrings  read GetItems;
    property ItemIndex  : Integer   read GetItemIndex  write SetItemIndex;
  end;

procedure Register;

implementation

uses
  UErrorHandlingOperations;

procedure Register;
begin
  RegisterComponents('WRMF', [TWRMFComboBox]);
end;

constructor TWRMFComboBox.Create(AOwner: TComponent);
const OPNAME = 'TWRMFComboBox.Create';
begin
  try
    inherited Create(AOwner);

    // Create the combo box.
    FComboBox := TComboBox.Create(self);
    FComboBox.Parent := self;
    FComboBox.Style  := csDropDownList;
    FComboBox.Top    := 0;
    FComboBox.Left   := 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFComboBox.Resize;
const OPNAME = 'TWRMFComboBox.Create';
begin
  try
    inherited Resize;
    ClientHeight   := FComboBox.Height;
    FComboBox.Width := ClientWidth - 10;
    FParamChangeIndicator.Left := ClientWidth - 10;
    FMetaDataIndicator.Left    := ClientWidth - 10;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFComboBox.SetFocus;
const OPNAME = 'TWRMFComboBox.SetFocus';
begin
  try
    FComboBox.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFComboBox.GetItemIndex : Integer;
const OPNAME = 'TWRMFComboBox.GetItemIndex';
begin
  Result := 0;
  try
    Result := FComboBox.ItemIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFComboBox.SetItemIndex(AIndex: Integer);
const OPNAME = 'TWRMFComboBox.SetItemIndex';
begin
  try
    FComboBox.ItemIndex := AIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFComboBox.GetItems : TStrings;
const OPNAME = 'TWRMFComboBox.GetItems';
begin
  Result := nil;
  try
    Result := FComboBox.Items;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFComboBox.GetText : String;
const OPNAME = 'TWRMFComboBox.GetText';
begin
  try
    Result := FComboBox.Text;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFComboBox.SetText(AText: string);
const OPNAME = 'TWRMFComboBox.SetText';
begin
  try
    FComboBox.Text := AText;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFComboBox.GetColor : TColor;
const OPNAME = 'TWRMFComboBox.GetColor';
begin
  Result := 0;
  try
    Result := FComboBox.Color;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFComboBox.SetColor(AColor: TColor);
const OPNAME = 'TWRMFComboBox.SetColor';
begin
  try
    FComboBox.Color := AColor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFComboBox.SetActive(AActive: Boolean);
const OPNAME = 'TWRMFComboBox.SetActive';
begin
  try
    FComboBox.Enabled := AActive;
    inherited SetActive(AActive);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFComboBox.ChangeColor;
const OPNAME = 'TWRMFComboBox.ChangeColor';
begin
  try
    if (NOT FIsValid) then
      FComboBox.Color := clRed
    else if (NOT FComboBox.Enabled) then
      FComboBox.Color := clSilver
    else
      FComboBox.Color := clWindow;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFComboBox.GetOnChange: TNotifyEvent;
const OPNAME = 'TWRMFComboBox.GetOnChange';
begin
  try
    Result := FComboBox.OnChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFComboBox.SetOnChange(AOnChange: TNotifyEvent);
const OPNAME = 'TWRMFComboBox.SetOnChange';
begin
  try
    FComboBox.OnChange := AOnChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFComboBox.GetOnExit: TNotifyEvent;
const OPNAME = 'TWRMFComboBox.GetOnExit';
begin
  try
    Result := FComboBox.OnExit;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFComboBox.SetOnExit(AOnChange: TNotifyEvent);
const OPNAME = 'TWRMFComboBox.SetOnExit';
begin
  try
    FComboBox.OnExit := AOnChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
