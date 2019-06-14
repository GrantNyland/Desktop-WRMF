unit UExplodePanel;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.ExtCtrls;

type
  TabAlignSet = (TabTop, TabBottom, TabLeft, TabRight);
  TabState = (tsOpen, tsClose);
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TExplodePanel = class(TCustomPanel)
  protected
    FCaptionHeight : integer;
    FPanelHeight   : integer;
    FTabAlignment  : TabAlignSet;
    FTabCaption    : String;
    FSpareAlign    : TAlign;
    FTopTab        : TPanel;
    FState         : TabState;

    procedure SetTabAlignment(Value : TabAlignSet);
    procedure SetTabCaption(Value : String);
    procedure SetState(Value : TabState);
  public
    constructor Create(AOwner : TComponent); override;
    procedure TopTabMouseUp(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
  published
    property Align;
    property Anchors;
    property PanelHeight: integer read FPanelHeight write FPanelHeight;
    property TabAlignment: TabAlignSet read FTabAlignment write SetTabAlignment;
    property TabCaption: string read FTabCaption write SetTabCaption;
    property CaptionHeight: integer read FCaptionHeight write FCaptionHeight;
    property State: TabState read FState write SetState;
  end;

procedure Register;

implementation

constructor TExplodePanel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  FCaptionHeight := 20;
  FTabAlignment := TabTop;
  FSpareAlign := alNone;
  //Tab Components
  FTopTab := TPanel.Create(Self);
  FTopTab.Height := FCaptionHeight;
  FTopTab.BevelInner := bvNone;
  FTopTab.BevelOuter := bvNone;
  FTopTab.Parent := Self;
  FTopTab.Align := alTop;
  FTopTab.Color := clHighlight;
  FTopTab.Font.Color := clHighlightText;
  FPanelHeight := 90;
  FTopTab.Alignment := taLeftJustify;
  FTopTab.OnMouseUp := TopTabMouseUp;
  FTopTab.Cursor := crSizeNS;
end;

procedure TExplodePanel.SetTabAlignment(Value : TabAlignSet);
begin
  if Value <> FTabAlignment then
  begin
    FTabAlignment := Value;
    case FTabAlignment of
      TabTop : begin
                 FTopTab.Align := alTop;
                 FTopTab.Height := FCaptionHeight;
               end;
      TabBottom  : begin
                 FTopTab.Align := alBottom;
                 FTopTab.Height := FCaptionHeight;
               end;
      TabLeft  : begin
                 FTopTab.Align := alLeft;
                 FTopTab.Width := FCaptionHeight;
               end;
      TabRight  : begin
                 FTopTab.Align := alRight;
                 FTopTab.Width := FCaptionHeight;
               end;
    end;//case
  end;//if Value
end;//procedure SetTabAlignment

procedure TExplodePanel.SetTabCaption(Value : String);
begin
  FTabCaption := Value;
  FTopTab.Caption := Value;
end;

procedure TExplodePanel.SetState(Value : TabState);

  procedure SetAlignment;
  begin
    case FTabAlignment of
      TabTop : Align := alTop;
      TabBottom : Align := alBottom;
      TabLeft : Align := alLeft;
      TabRight : Align := alRight;
    end;//case
  end;

begin
  if FState <> Value then
  begin
    if Value = tsClose then
    begin
      FTopTab.Color := clInactiveBorder;
      FTopTab.Font.Color := clInactiveCaption;
      FState := tsClose;
      if Align <> alNone then
      begin
        FSpareAlign := Align;
        SetAlignment;
      end;
      if FTabAlignment in [TabTop, TabBottom] then
      begin
        FPanelHeight := Self.Height;
        Self.Height := FCaptionHeight + FTopTab.Top;
      end
      else
      begin
        FPanelHeight := Self.Width;
        Self.Width := FCaptionHeight;
      end;
    end
    else
    begin
      FTopTab.Color := clHighlight;
      FTopTab.Font.Color := clHighlightText;
      FState := tsOpen;
      if Align <> alNone then
        Align := FSpareAlign;
      if FTabAlignment in [TabTop, TabBottom] then
        Self.Height := FPanelHeight
      else
        Self.Width := FPanelHeight;
      FTopTab.Tag := 0;
    end;
  end;
end;

procedure TExplodePanel.TopTabMouseUp(Sender : TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
  if FState = tsOpen then
    SetState(tsClose)
  else
    SetState(tsOpen)
end;

procedure Register;
begin
  RegisterComponents('Stomsa', [TExplodePanel]);
end;

end.
