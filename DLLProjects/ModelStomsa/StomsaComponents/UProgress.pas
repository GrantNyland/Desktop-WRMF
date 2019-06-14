unit UProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.stdctrls;

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TProgress = class(TCustomPanel)
  protected
    ContainerPanel : TPanel;
    ProgressBar    : TPanel;
    TopCaption,
    BottomCaption  : TLabel;
    FPosition : single;

    procedure SetPosition(Value : single);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    Constructor Create(AOwner : TComponent); override;
    { Public declarations }
  published
    { Published declarations }
    property Align;
    property Position : single read FPosition write SetPosition;
  end;

procedure Register;

implementation

Constructor TProgress.Create(AOwner : TComponent);
begin
  inherited;
  Self.Caption := '';
  Self.BevelOuter := bvNone;
  Self.BevelInner := bvNone;
  Self.Width := 35;
  Self.Height := 70;

  FPosition := 50.0;

  TopCaption := TLabel.Create(self);
  TopCaption.Caption := '100%';
  TopCaption.Align := alTop;
  TopCaption.Alignment := taCenter;
  TopCaption.Parent := self;

  BottomCaption := TLabel.Create(self);
  BottomCaption.Caption := '0%';
  BottomCaption.Align := alBottom;
  BottomCaption.Alignment := taCenter;
  BottomCaption.Parent := self;

  ContainerPanel := TPanel.Create(self);
  ContainerPanel.Align := alClient;
  ContainerPanel.BevelInner := bvLowered;
  ContainerPanel.BevelOuter := bvRaised;
  ContainerPanel.Parent := self;

  ProgressBar := TPanel.Create(self);
  ProgressBar.BevelInner := bvNone;
  ProgressBar.BevelOuter := bvNone;
  ProgressBar.Align := alBottom;
  ProgressBar.Color := clBlue;
//  ProgressBar.height := round((ContainerPanel.Height - 4*ContainerPanel.BevelWidth)*0.5);
  ProgressBar.Parent := ContainerPanel;
end;

procedure TProgress.SetPosition(Value : single);
begin
  if Value <> FPosition then
  begin
    if Value > 100 then
      Value := 100;
    FPosition := Value;
    ProgressBar.Height := round((ContainerPanel.Height - 4*ContainerPanel.BevelWidth)*FPosition/100);
  end;
end;

procedure TProgress.WMSize(var Message: TWMSize);
begin
  ContainerPanel.Align := alClient;
  ProgressBar.Height := round((ContainerPanel.Height - 4*ContainerPanel.BevelWidth)*FPosition/100);
end;

procedure Register;
begin
  RegisterComponents('Stomsa', [TProgress]);
end;



end.
