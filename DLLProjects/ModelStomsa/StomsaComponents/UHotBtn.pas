unit UHotBtn;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.ExtCtrls, VCL.StdCtrls;

type
  TextPos = (TopPos, BottomPos, LeftPos, RightPos);
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  THotBtn = class(TCustomPanel)
  protected
    FTheImage   : TImage;
    FLabel      : TLabel;
    FText       : string;
    FUpState    : boolean;
    FEnabled    : boolean;
    FImageAlign : TAlign;
    FHotGlyph   : TBitmap;
    FPlainGlyph : TBitmap;
    FDisabledGlyph : TBitmap;

    procedure SetHotGlyph(Value: TBitmap);
    procedure SetPlainGlyph(Value: TBitmap);
    procedure SetDisabledGlyph(Value: TBitmap);
    procedure SetText(value : string);
    procedure SetImageAlign(Value : TAlign);
    procedure TCMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure TCMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure TWMPaint(var Message : TWMPaint); message WM_PAINT;

    procedure Click; override;
    procedure MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure SetEnabled(Value : boolean); override;
    procedure MseDwn(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure MseUp(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure MseMve(Sender : TObject; Shift: TShiftState; X, Y: Integer);
    procedure Clck(Sender : TObject);

    property TheLabel : TLabel read FLabel write FLabel;
    property TheImage : TImage read FTheImage write FTheImage;
    property UpState : boolean read FUpState write FUpState;
  published
    //sl 2000.08.04 - Add ACTION property to tie into the ActionList component
    property Action;
    property Align;
    property Anchors;
    property BevelOuter;
    property Enabled: boolean read FEnabled write SetEnabled;
    property ShowHint;
    property Visible;

    property GlyphHot: TBitmap read FHotGlyph write SetHotGlyph;
    property GlyphPlain: TBitmap read FPlainGlyph write SetPlainGlyph;
    property GlyphDisabled: TBitmap read FDisabledGlyph write SetDisabledGlyph;

    property Text : string read FText write SetText;
    property ImageAlign : TAlign read FImageAlign write SetImageAlign;

    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
  end;

procedure Register;

implementation

constructor THotBtn.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Width := 70;
  Height := 52;
  UpState := false;
  BevelOuter := bvNone;
  caption := '';

  //should actually draw everything on a canvas (text + image)
  //can then control alignment myself, however can't disenable stuff

  TheLabel := TLabel.Create(self);
  Thelabel.Parent := Self;
  TheLabel.Caption := text;
  TheLabel.Alignment := taCenter;
  TheLabel.Align := alClient;
  TheLabel.OnMouseDown := MseDwn;
  TheLabel.OnMouseUp := MseUp;
  TheLabel.OnMouseMove := MseMve;
  TheLabel.OnClick := Clck;
  TheLabel.Enabled := true;

  TheImage := TImage.Create(self);
  TheImage.Parent := Self;
  TheImage.Center := true;
  TheImage.Transparent := true;
  TheImage.OnMouseDown := MseDwn;
  TheImage.OnMouseUp := MseUp;
  TheImage.OnMouseMove := MseMve;
  TheImage.OnClick := Clck;
  TheImage.Enabled := true;

  Enabled := true;

  ImageAlign := alTop;

  FHotGlyph := TBitmap.Create;
  FPlainGlyph := TBitmap.Create;
  FDisabledGlyph := TBitmap.Create;
end;

procedure THotBtn.SetEnabled(Value : boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    TheImage.Enabled := Value;
    TheLabel.Enabled := Value;
  end;
end;

procedure THotBtn.SetImageAlign(Value : TAlign);
begin
  FImageAlign := Value;
  TheImage.Align := Value;
  case Value of
    alTop,alBottom : begin
                       TheImage.Height := Self.Height-15;// FPlainGlyph.Height + 6
                       TheLabel.Layout := tlCenter;
                       TheLabel.Alignment := taCenter;
                     end;
    alLeft,alRight : begin
                       if FPlainGlyph <> nil then
                         TheImage.Width := FPlainGlyph.Width + 6
                       else
                         TheImage.Width := 10;
                       TheLabel.Layout := tlCenter;
                     end;
    alClient       : begin
                       TheLabel.Alignment := taCenter;
                       TheLabel.Layout := tlCenter;
                     end;
  end;//case
end;

procedure THotBtn.MseDwn(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
  MouseDown(Button,Shift,X,Y);
end;

procedure THotBtn.MseUp(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
  MouseUp(Button,Shift,X,Y);
end;

procedure THotBtn.MseMve(Sender : TObject;Shift: TShiftState; X, Y: Integer);
begin
  MouseMove(Shift,X,Y);
end;

procedure THotBtn.Clck(Sender : TObject);
begin
  Click
end;

procedure THotBtn.SetHotGlyph(Value: TBitmap);
begin
  Invalidate;
  FHotGlyph.Assign(Value);
end;

procedure THotBtn.SetPlainGlyph(Value: TBitmap);
begin
  FPlainGlyph.Assign(Value);
  ImageAlign := ImageAlign;
  Invalidate;
end;

procedure THotBtn.SetDisabledGlyph(Value: TBitmap);
begin
  FDisabledGlyph.Assign(Value);
  Invalidate;
end;

procedure THotBtn.SetText(Value : string);
begin
  if FText <> value then
  begin
    FLabel.Caption := Value;
    FText := Value;
  end;
end;

procedure THotBtn.Click;
begin
  IF FEnabled then
  begin
    inherited Click;
  end;
end;

procedure THotBtn.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
  IF FEnabled then
  begin
    BevelOuter := bvLowered;
    inherited MouseDown(Button,Shift,x,y);
  end;
end;

procedure THotBtn.MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
  if FEnabled then
  begin
    BevelOuter := bvRaised;
    inherited MouseUp(Button,Shift,x,y);
  end;
end;

procedure THotBtn.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FEnabled then
  begin
    BevelOuter := bvRaised;
    inherited MouseMove(Shift,x,y);
  end;
end;

procedure THotBtn.TWMPaint(var Message : TWMPaint);
begin
  Caption := '';
  if Enabled then
    if UpState then
    begin
      FHotGlyph.TransparentMode := tmAuto;
      TheImage.Picture.Bitmap := FHotGlyph;
    end
    Else
    begin
      FPlainGlyph.TransparentMode := tmAuto;
      TheImage.Picture.Bitmap := FPlainGlyph;
    end
  else
  begin
    FDisabledGlyph.TransparentMode := tmAuto;
    BevelOuter := bvNone;    
    TheImage.Picture.Bitmap := FDisabledGlyph;
  end;
  inherited;
end;

procedure THotBtn.TCMMouseEnter(var Message : TMessage);
begin
  if FEnabled then
  begin
    UpState := true;
    BevelOuter := bvRaised;
    inherited;
  end;
end;

procedure THotBtn.TCMMouseLeave(var Message : TMessage);
begin
  if FEnabled then
  begin
    UpState := false;
    BevelOuter := bvNone;
    inherited;
  end;
end;


procedure Register;
begin
  RegisterComponents('Stomsa', [THotBtn]);
end;

end.
