unit UExpandPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TExpandPanel = class(TCustomPanel)
  protected
    FTopPanel : TPanel;
    FExpanded : Boolean;
    FTopColor : TColor;
    FTopHeight : Integer;
    FTopCaption : TCaption;
    FFullHeight : integer;
    FOnExpand : TNotifyEvent;

    procedure SetExpanded(Value : Boolean);
    procedure SetFullHeight(Value : Integer);
    procedure SetTopColor(Value : TColor);
    procedure SetTopHeight(Value : Integer);
    procedure SetTopCaption(Value : TCaption);
  public
    Constructor Create(AOwner : TComponent); override;
  published
    Property Align;
    Property Anchors;
    Property BevelInner;
    Property BevelOuter;
    Property BevelWidth;
    Property Caption;
    Property Color;
    Property TabOrder;
    property Expanded: Boolean Read FExpanded Write SetExpanded;
    property FullHeight: integer Read FFullHeight Write SetFullHeight;
    property TopCaption: TCaption Read FTopCaption Write SetTopCaption;
    Property TopColor: TColor Read FTopColor Write SetTopColor;
    property TopHeight: Integer Read FTopHeight Write SetTopHeight;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    procedure OnTopPanelClick(Sender : TObject);
  end;

procedure Register;

implementation

procedure TExpandPanel.OnTopPanelClick(Sender : TObject);
begin
  Self.Expanded := NOT(Self.Expanded); 
end;

procedure TExpandPanel.SetExpanded(Value : Boolean);
begin
  if Value <> FExpanded then
  begin
    FExpanded := Value;
    if FExpanded = true then
    begin
      Self.Height := Self.FFullHeight;
    end
    else
    begin
      Self.Height := Self.FTopHeight;
    end;
    if Assigned(OnExpand) then
      OnExpand(Self);
    Self.Refresh;
  end;
end;

procedure TExpandPanel.SetFullHeight(Value : Integer);
begin
  if Value <> FFullHeight then
  begin
    FFullHeight := Value;
    if Self.Expanded then
    begin
      Self.Height := FFullHeight;
      Self.Refresh;
    end;
  end;
end;

procedure TExpandPanel.SetTopCaption(Value : TCaption);
begin
  if Value <> FTopCaption then
  begin
    FTopCaption := Value;
    FTopPanel.Caption := FTopCaption;
    FTopPanel.Refresh;
  end;
end;

procedure TExpandPanel.SetTopHeight(Value : Integer);
begin
  if Value <> FTopHeight then
  begin
    FTopHeight := Value;
    FTopPanel.Height := FTopHeight;
    FTopPanel.Refresh;
  end;
end;

procedure TExpandPanel.SetTopColor(Value : TColor);
begin
  if Value <> FTopColor then
  begin
    FTopColor := Value;
    FTopPanel.Color := FTopColor;
    FTopPanel.Refresh;
  end;
end;

Constructor TExpandPanel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  Self.BorderStyle := bsSingle;
  FTopPanel := TPanel.Create(Self);
  FTopPanel.BevelInner := bvNone;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.Align := alTop;
  FTopPanel.BorderStyle := bsSingle;
  FTopPanel.Alignment := taLeftJustify;
  FTopPanel.OnClick := OnTopPanelClick;
  FTopPanel.Parent := Self;

  FullHeight := 100;
  TopColor := clTeal;
  TopHeight := 20;
  Expanded := True;
  Self.Height := TopHeight;
end;


procedure Register;
begin
  RegisterComponents('Stomsa', [TExpandPanel]);
end;

end.
