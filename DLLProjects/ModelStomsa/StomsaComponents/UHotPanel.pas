unit UHotPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  THotPanel = class(TCustomPanel)
  protected
    FEnabled    : boolean;
    FState      : boolean;
    FTheImage   : TImage;
    FHotGlyph   : TBitmap;
    FPlainGlyph : TBitmap;
    FFirst      : boolean;

    procedure SetEnabled(Value : boolean); override;
    procedure SetHotGlyph(Value: TBitmap);
    procedure SetPlainGlyph(Value: TBitmap);
    procedure TCMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure TCMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure TWMPaint(var Message : TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner : TComponent); override;
    property TheImage : TImage read FTheImage write FTheImage;
  published
    property Enabled: boolean read FEnabled write SetEnabled;
    property State: boolean read FState write FState;
    property GlyphHot: TBitmap read FHotGlyph write SetHotGlyph;
    property GlyphPlain: TBitmap read FPlainGlyph write SetPlainGlyph;
  end;

procedure Register;

implementation

constructor THotPanel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Enabled := true;

  BevelOuter := bvNone;
  Caption := '';

  FFirst := true;

  TheImage := TImage.Create(self);
  TheImage.Parent := Self;
  TheImage.Center := true;
  TheImage.Align := alClient;
  TheImage.Enabled := true;
    
  FHotGlyph := TBitmap.Create;
  FPlainGlyph := TBitmap.Create;
end;

procedure THotPanel.SetEnabled(Value : boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
  end;
end;

procedure THotPanel.TWMPaint(var Message : TWMPaint);
begin
  caption := '';
  if FFirst then
  begin
    TheImage.Picture.Bitmap.Assign(FPlainGlyph);
    FFirst := false;
  end;
  inherited;
end;

procedure THotPanel.TCMMouseEnter(var Message : TMessage);
begin
  if Enabled then
  begin
    TheImage.Picture.Bitmap.Assign(FHotGlyph);
    inherited;
  end;
end;

procedure THotPanel.TCMMouseLeave(var Message : TMessage);
begin
  if Enabled then
  begin
    TheImage.Picture.Bitmap.Assign(FPlainGlyph);
    inherited;
  end;
end;

procedure THotPanel.SetHotGlyph(Value: TBitmap);
begin
  FHotGlyph.Assign(Value);
end;

procedure THotPanel.SetPlainGlyph(Value: TBitmap);
begin
  FPlainGlyph.Assign(Value);
  TheImage.Picture.Bitmap.Assign(Value);
  TheImage.Invalidate;
end;

procedure Register;
begin
  RegisterComponents('Stomsa', [THotPanel]);
end;

end.
