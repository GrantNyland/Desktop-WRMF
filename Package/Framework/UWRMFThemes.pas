//
//
//  UNIT      : Contains themes(skin) Classes
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 23/10/2012
//  COPYRIGHT : Copyright © 2012 DWAF
//
//
unit UWRMFThemes;

interface

uses
  //  Delphi VCL
  Classes, Windows,SysUtils,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  VCL.Forms,
  Winapi.Messages,

  Vcl.Themes,
  Vcl.Styles,

  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent;

type
  //TWinControlH = class(TWinControl);
  TFieldEditH  = class(TWinControl);

  TEditStyleHookColor = class(TEditStyleHook)
  private
    procedure UpdateColors;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TChannelPenaltyPanel = class(TAbstractPanel)
  private
    FBevelLowColor: TColor;
    FBevelHiColor: TColor;
    procedure SetBvlLowColor(Value: TColor);
    procedure SetBvlHiColor(Value: TColor);
    { Private declarations }
  protected
    FIDLabel          : TLabel;
    FDescriptionEdit  : TFieldEdit;
    FArcPenaltiesGrid : TFieldStringGrid;
    FPenaltyNumber    : integer;
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create (AOwner : TComponent; AAppModules: TAppModules); override;
    procedure SetEventHandlerOnDragOver(AOnDragOver: TDragOverEvent);
    procedure SetEventHandlerOnDragDrop(AOnDragDrop: TDragDropEvent);
    property IDLabel          : TLabel           read FIDLabel;
    property DescriptionEdit  : TFieldEdit       read FDescriptionEdit;
    property ArcPenaltiesGrid : TFieldStringGrid read FArcPenaltiesGrid;
    property PenaltyNumber    : integer          read FPenaltyNumber  write FPenaltyNumber;
    property BevelShadowColor : TColor           read FBevelLowColor  write SetBvlLowColor;
    property BevelHighlightColor : TColor        read FBevelHiColor   write SetBvlHiColor;
  end;

  TWRMFThemesManager = class(TObject)
  public
    class function InitialiseThemes(AActivate : boolean = True): boolean;
    class function IsThemesActive: boolean;
    class function SetChannelPenaltyPanelDefaultColor(APanel : TChannelPenaltyPanel): boolean;
    class function GetPanelDefaultColor: TColor;
  end;

implementation

uses
  System.UITypes,
  UHelpContexts,
  UErrorHandlingOperations;
{******************************************************************************}
{* TChannelPenaltyPanel                                                       *}
{******************************************************************************}

constructor TChannelPenaltyPanel.Create (AOwner      : TComponent; AAppModules : TAppModules);
var
  LStyle   : TCustomStyleServices;
  LDetails : TThemedElementDetails;
  LColor   : TColor;
begin
  inherited;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  Width := 185;
  Height := 41;
  BevelOuter := bvRaised;
  BevelOuter := bvLowered;
  BevelWidth := 1;
  FBevelLowColor := clBtnShadow;
  FBevelHiColor := clBtnHighlight;

  LColor := clBtnFace;
  LStyle := StyleServices;
  if LStyle.Enabled then
  begin
      LDetails := LStyle.GetElementDetails(tpPanelBackground);
      LStyle.GetElementColor(LDetails,ecFillColor,LColor);
  end;
  Color := LColor;

  FIDLabel := TLabel.Create(Self);
  with FIDLabel do
  begin
    Parent      := Self;
    Left        := 5;
    Top         := 11;
    Width       := 36;
    Height      := 13;
    Caption     := '';
    Font.Color  := clWindowText;
    Font.Height := -11;
    Font.Name   := 'MS Sans Serif';
    Font.Style  := [fsBold];
    ParentFont  := False;
    HelpContext := HC_ChannelPenaltyStructures;
  end;
  FDescriptionEdit := TFieldEdit.Create(Self, FAppModules);
  with FDescriptionEdit do
  begin
    Parent      := Self;
    Left        := 45;
    Top         := 7;
    Width       := 180;
    Height      := 21;
    TabOrder    := 0;
    Tabstop     := TRUE;
    HelpContext := HC_ChannelPenaltyStructures;
  end;
  FArcPenaltiesGrid := TFieldStringGrid.Create(Self, FAppModules);
  with FArcPenaltiesGrid do
  begin
    Parent           := Self;
    Left             := 230;
    Top              := 5;
    Width            := 258;
    Height           := 24;
    TabOrder         := 1;
    TabStop          := TRUE;
    ColCount         := 5;
    RowCount         := 1;
    DefaultColWidth  := 50;
    DefaultRowHeight := 21;
    FixedCols        := 0;
    FixedRows        := 0;
    ScrollBars       := ssNone;
    TabOrder         := 1;
    HelpContext      := HC_ChannelPenaltyStructures;
  end;
end;

procedure TChannelPenaltyPanel.Paint;
const OPNAME = 'TChannelPenaltyPanel.Paint';
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  FontHeight: Integer;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := FBevelHiColor;
    if Bevel = bvLowered then TopColor := FBevelLowColor;
    BottomColor := FBevelLowColor;
    if Bevel = bvLowered then BottomColor := FBevelHiColor;
  end;

begin
  try
    Rect := GetClientRect;
    if BevelOuter <> bvNone then
    begin
      AdjustColors(BevelOuter);
      Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
    end;
    Frame3D(Canvas, Rect, Color, Color, BorderWidth);
    if BevelInner <> bvNone then
    begin
      AdjustColors(BevelInner);
      Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
    end;
    with Canvas do
    begin
      Brush.Color := Color;
      FillRect(Rect);
      Brush.Style := bsClear;
      Font := Self.Font;
      FontHeight := TextHeight('W');
      with Rect do
      begin
        Top := ((Bottom + Top) - FontHeight) div 2;
        Bottom := Top + FontHeight;
      end;
      DrawText(Handle, PChar(Caption), -1, Rect, (DT_EXPANDTABS or
        DT_VCENTER) or Alignments[Alignment]);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyPanel.SetBvlHiColor(Value: TColor);
begin
  if FBevelHiColor <> Value then
    FBevelHiColor := Value;
  Invalidate;
end;

procedure TChannelPenaltyPanel.SetBvlLowColor(Value: TColor);
begin
  if FBevelLowColor <> Value then
    FBevelLowColor := Value;
  Invalidate;
end;

procedure TChannelPenaltyPanel.SetEventHandlerOnDragDrop(AOnDragDrop: TDragDropEvent);
const OPNAME = 'TChannelPenaltyPanel.SetEventHandlerOnDragDrop';
begin
  try
    Self.OnDragDrop               :=  AOnDragDrop;
    FIDLabel.OnDragDrop           :=  AOnDragDrop;
    FDescriptionEdit.OnDragDrop   :=  AOnDragDrop;
    FArcPenaltiesGrid.OnDragDrop  :=  AOnDragDrop;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChannelPenaltyPanel.SetEventHandlerOnDragOver(AOnDragOver: TDragOverEvent);
const OPNAME = 'TChannelPenaltyPanel.SetEventHandlerOnDragOver';
begin
  try
    Self.OnDragOver               :=  AOnDragOver;
    FIDLabel.OnDragOver           :=  AOnDragOver;
    FDescriptionEdit.OnDragOver   :=  AOnDragOver;
    FArcPenaltiesGrid.OnDragOver  :=  AOnDragOver;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


{ TWRMFThemesManager }


class function TWRMFThemesManager.InitialiseThemes(AActivate : boolean = True): boolean;
const OPNAME = 'TWRMFThemesManager.InitialiseThemes';
begin
  Result := True;
  try
    if AActivate then
    begin
      TStyleManager.TrySetStyle('Emerald Light Slate');
      TStyleManager.Engine.RegisterStyleHook(TStatusBar, TStyleHook);
      TStyleManager.Engine.RegisterStyleHook(TRichEdit, TStyleHook);
      TStyleManager.Engine.RegisterStyleHook(TTreeView, TStyleHook);
      TStyleManager.Engine.RegisterStyleHook(TEdit, TEditStyleHookColor);
    end
    else
    begin
      TStyleManager.TrySetStyle('');
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

class function TWRMFThemesManager.IsThemesActive: boolean;
const OPNAME = 'TWRMFThemesManager.IsThemesActive';
begin
  Result := False;
  try
    Result := StyleServices.Enabled;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

class function TWRMFThemesManager.GetPanelDefaultColor: TColor;
const OPNAME = 'TWRMFThemesManager.GetPanelDefaultColor';
var
  LStyle        : TCustomStyleServices;
  LDetails      : TThemedElementDetails;
begin
  Result := clBtnShadow;
  try
    if IsThemesActive then
    begin
      LStyle := StyleServices;
      LDetails := LStyle.GetElementDetails(tpPanelBevel);
      LStyle.GetElementColor(LDetails,ecFillColor,Result);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

class function TWRMFThemesManager.SetChannelPenaltyPanelDefaultColor(APanel: TChannelPenaltyPanel): boolean;
const OPNAME = 'TWRMFThemesManager.SetChannelPenaltyPanelDefaultColor';
var
  LColor        : TColor;
begin
  Result := False;
  try
    LColor := GetPanelDefaultColor;
    APanel.ArcPenaltiesGrid.Col := 0;
    APanel.BevelShadowColor := LColor;
    APanel.BevelHighlightColor := LColor;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//__________________________________________________________________________________________________________________

constructor TEditStyleHookColor.Create(AControl: TWinControl);
const OPNAME = 'TEditStyleHookColor.Create';
begin
  inherited;
  try
    //call the UpdateColors method to use the custom colors
    UpdateColors;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//Here you set the colors of the style hook
procedure TEditStyleHookColor.UpdateColors;
const OPNAME = 'TEditStyleHookColor.UpdateColors';
var
  LStyle: TCustomStyleServices;
begin
  try
    if Control.Enabled then
    begin
      //Brush.Color := TWinControlH(Control).Color; //use the Control color
      //FontColor   := TWinControlH(Control).Font.Color;//use the Control font color
      Brush.Color := TFieldEditH(Control).Color; //use the Control color
      FontColor   := TFieldEditH(Control).Font.Color;//use the Control font color
    end
    else
    begin
      //if the control is disabled use the colors of the style
      LStyle := StyleServices;
      Brush.Color := LStyle.GetStyleColor(scEditDisabled);
      FontColor := LStyle.GetStyleFontColor(sfEditBoxTextDisabled);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

//Handle the messages of the control
procedure TEditStyleHookColor.WndProc(var Message: TMessage);
const OPNAME = 'TEditStyleHookColor.WndProc';
begin
  try
    case Message.Msg of
      CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
        begin
          //Get the colors
          UpdateColors;
          SetTextColor(Message.WParam, ColorToRGB(FontColor));
          SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
          Message.Result := LRESULT(Brush.Handle);
          Handled := True;
        end;
      CM_ENABLEDCHANGED:
        begin
          //Get the colors
          UpdateColors;
          Handled := False;
        end
    else
      inherited WndProc(Message);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.




