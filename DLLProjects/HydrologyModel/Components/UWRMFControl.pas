unit UWRMFControl;

interface

uses
  SysUtils, Classes, VCL.Controls, VCL.StdCtrls, VCL.ExtCtrls, VCL.Graphics;

type
  TWRMFControl = class(TPanel)
  protected
    { Protected declarations }
    FParamChangeIndicator : TShape;
    FMetaDataIndicator    : TShape;
    FPropertyName         : String;
    FHasParamChange       : Boolean;
    FHasMetaData          : Boolean;
    FIsValid              : Boolean;
    FAssociatedLabel      : TLabel;
    FOnParamChangeClick   : TNotifyEvent;
    FOnMetaDataClick      : TNotifyEvent;
    procedure Resize; override;
    procedure SetHasParamChange (AHasParamChange : Boolean);
    procedure SetHasMetaData (AHasMetaData : Boolean);
    procedure SetIsValid (AIsValid : Boolean);
    procedure ChangeColor; virtual; abstract;
    function GetText : string; virtual;
    procedure SetText (AText : string); virtual;
    function GetColor : TColor; virtual;
    procedure SetColor (AColor : TColor); virtual;
    function GetActive : Boolean; virtual;
    procedure SetActive (AActive : Boolean); virtual;
    function GetOnChange: TNotifyEvent; virtual; abstract;
    procedure SetOnChange(AOnChange: TNotifyEvent); virtual; abstract;
    function GetOnExit: TNotifyEvent; virtual; abstract;
    procedure SetOnExit(AOnExit: TNotifyEvent); virtual; abstract;
    procedure DoParamChangeIndicatorClicked (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMetaDataIndicatorClicked (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    property Caption;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
  published
    { Published declarations }
    property Color                : TColor       read GetColor            write SetColor;
    property Active               : Boolean      read GetActive           write SetActive;
    property Text                 : string       read GetText             write SetText;
    property OnChange             : TNotifyEvent read GetOnChange         write SetOnChange;
    property OnExit               : TNotifyEvent read GetOnExit           write SetOnExit;
    property PropertyName         : String       read FPropertyName       write FPropertyName;
    property HasParamChange       : Boolean      read FHasParamChange     write SetHasParamChange;
    property HasMetaData          : Boolean      read FHasMetaData        write SetHasMetaData;
    property IsValid              : Boolean      read FIsValid            write SetIsValid;
    property AssociatedLabel      : TLabel       read FAssociatedLabel    write FAssociatedLabel;
    property OnParamChangeClick   : TNotifyEvent read FOnParamChangeClick write FOnParamChangeClick;
    property OnMetaDataClick      : TNotifyEvent read FOnMetaDataClick    write FOnMetaDataClick;
  end;


implementation

uses
  UErrorHandlingOperations;

{ TWRMFControl ***************************************************************}

constructor TWRMFControl.Create(AOwner: TComponent);
const OPNAME = 'TWRMFControl.Create';
begin
  try
    inherited Create(AOwner);

    // Set parent panel properties.
    BevelInner := bvNone;
    BevelOuter := bvNone;
    
    // Create the Param Change indicator.
    FParamChangeIndicator := TShape.Create(self);
    FParamChangeIndicator.Parent  := self;
    FParamChangeIndicator.Top     := 0;
    FParamChangeIndicator.Width   := 10;
    FParamChangeIndicator.Height  := 10;
    FParamChangeIndicator.Pen.Color   := clDkGray;
    FParamChangeIndicator.Brush.Color := clSilver;
    FParamChangeIndicator.Shape       := stRoundSquare;
    FParamChangeIndicator.OnMouseDown := DoParamChangeIndicatorClicked;

    // Create the Param Change indicator.
    FMetaDataIndicator := TShape.Create(self);
    FMetaDataIndicator.Parent  := self;
    FMetaDataIndicator.Top     := 11;
    FMetaDataIndicator.Width   := 10;
    FMetaDataIndicator.Height  := 10;
    FMetaDataIndicator.Pen.Color   := clDkGray;
    FMetaDataIndicator.Brush.Color := clSilver;
    FMetaDataIndicator.Shape       := stRoundSquare;
    FMetaDataIndicator.OnMouseDown := DoMetaDataIndicatorClicked;

    FHasParamChange  := FALSE;
    FHasMetaData     := FALSE;
    FIsValid         := TRUE;
    FAssociatedLabel := nil;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.Resize;
const OPNAME = 'TWRMFControl.Resize';
begin
  try
    if (Height < 20) then
      Height := 20;
    inherited Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.SetFocus;
const OPNAME = 'TWRMFControl.SetFocus';
begin
  try
    Self.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.SetHasParamChange(AHasParamChange: Boolean);
const OPNAME = 'TWRMFControl.SetHasParamChange';
begin
  try
    FHasParamChange := AHasParamChange;
    if (FHasParamChange) then
    begin
      FParamChangeIndicator.Brush.Color := clLime;
      FParamChangeIndicator.Pen.Color   := clBlack;
    end
    else
    begin
      FParamChangeIndicator.Brush.Color := clSilver;
      FParamChangeIndicator.Pen.Color   := clDkGray;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.SetHasMetaData(AHasMetaData: Boolean);
const OPNAME = 'TWRMFControl.SetHasMetaData';
begin
  try
    FHasMetaData := AHasMetaData;
    if (FHasMetaData) then
    begin
      FMetaDataIndicator.Brush.Color := clAqua;
      FMetaDataIndicator.Pen.Color   := clBlack;
    end
    else
    begin
      FMetaDataIndicator.Brush.Color := clSilver;
      FMetaDataIndicator.Pen.Color   := clDkGray;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFControl.GetText : String;
const OPNAME = 'TWRMFControl.GetText';
begin
  try
    Result := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.SetText(AText: string);
const OPNAME = 'TWRMFControl.SetText';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFControl.GetColor : TColor;
const OPNAME = 'TWRMFControl.GetColor';
begin
  Result := 0;
  try
    Result := Self.Color;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.SetColor(AColor: TColor);
const OPNAME = 'TWRMFControl.SetColor';
begin
  try
    Self.Color := AColor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWRMFControl.GetActive : Boolean;
const OPNAME = 'TWRMFControl.GetActive';
begin
  Result := FALSE;
  try
    Result := Enabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.SetActive(AActive: Boolean);
const OPNAME = 'TWRMFControl.SetActive';
begin
  try
    ChangeColor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.SetIsValid (AIsValid : Boolean);
const OPNAME = 'TWRMFControl.SetIsValid';
begin
  try
    FIsValid := AIsValid;
    ChangeColor;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.DoParamChangeIndicatorClicked (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TWRMFControl.DoParamChangeIndicatorClicked';
begin
  try
    if Assigned(FOnParamChangeClick) then
      FOnParamChangeClick(self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWRMFControl.DoMetaDataIndicatorClicked (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const OPNAME = 'TWRMFControl.DoMetaDataIndicatorClicked';
begin
  try
    if Assigned(FOnMetaDataClick) then
      FOnMetaDataClick(self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
