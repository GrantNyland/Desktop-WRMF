//
//
//  UNIT      : Contains TimeSeriesComparitorXYAxisReset Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 02/02/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UTimeSeriesComparitorXYAxisReset;

interface
uses
  VCL.controls,
  Classes,
  VCL.Forms,
  VCL.Grids,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.Buttons,
  VCL.StdCtrls,
  UAbstractComponent;

type

  TClickHandle = procedure ( Sender : TObject; var aMinXValue : double;
                             var aMinYValue : TDateTime; var aMaxXValue : double;
                             var aMaxYValue : TDateTime; var aIncValue : double ) of object;
  TTimeSeriesComparitorXYAxisReset = class ( TAbstractForm )
  protected
    FPnlButtons     : TPanel;

    FBtnOK          : TBitBtn;
    FBtnCancel      : TBitBtn;
    FBtnReset       : TBitBtn;

    FlblMinXValue   : TLabel;
    FlblMinYValue   : TLabel;
    FlblMaxXValue   : TLabel;
    FlblMaxYValue   : TLabel;
    FlblInc         : TLabel;

    FedtMinXValue   : TEdit;
    FedtMinYValue   : TEdit;
    FedtMaxXValue   : TEdit;
    FedtMaxYValue   : TEdit;
    FedtInc         : TEdit;
    FOnkeyPress     : TKeyPressEvent;
    FVerticalGroup  : TGroupBox;
    FHorizontalGroup: TGroupBox;
    FClickHandle    : TClickHandle;

    FMinXValue   : double;
    FMinYValue   : TDateTime;
    FMaxXValue   : double;
    FMaxYValue   : TDateTime;
    FInc         : double;

    function GetMinXValue : double;
    function GetMaxXValue : double;
    function GetIncValue : double;
    function GetMaxYValue : TDatetime;
    function GetMinYValue : TDateTime;
    procedure SetMaxXValue ( const aValue : double );
    procedure SetMaxYValue ( const aValue : TDateTime );
    procedure SetMinXValue ( const aValue : double );
    procedure SetMinYValue ( const aValue : TDateTime );
    procedure SetIncValue ( const aValue : double );
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure DoOnclick ( Sender : TObject );
  public
    procedure Resize; override;
    procedure SetEnabled(AEnabled : boolean);override;
    procedure SetOnKeyPress(AKeyPressEvent : TKeyPressEvent);
    procedure SetOnClickHandle ( aClickHandle : TClickHandle );
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function Initialise: boolean; override;
    property BtnReset : TBitBtn read FBtnReset write FBtnReset;
    property MinXValue  : double read GetMinXValue write SetMinXValue;
    property MinYValue  : TDateTime read GetMinYValue write SetMinYValue;
    property MaxXValue  : double read GetMaxXValue write SetMaxXValue;
    property MaxYValue  : TDateTime read GetMaxYValue write SetMaxYValue;
    property IncValue   : double read GetIncValue write SetIncValue;
    property OnKeyPress : TKeyPressEvent read FOnkeyPress write SetOnKeyPress;
    property OnClickHandle : TClickHandle read FClickHandle write SetOnClickHandle;

end;

implementation
uses
  SysUtils,
  VCL.Graphics,
  UTimeSeriesComparitorLinkClasses,
  UErrorHandlingOperations;
const
  cDefaultValue : string = '0.0';
  clblLeftSpace : integer = 5;
  cEdtSpace     : integer = 50;
{ TimeSeriesComparitorXYAxisReset }

procedure TTimeSeriesComparitorXYAxisReset.AssignHelpContext;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.AssignHelpContext';
begin
  inherited;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FVerticalGroup  := TGroupBox.Create ( Self );
    FHorizontalGroup:= TGroupBox.Create ( Self );

    FlblMinXValue   := TLabel.Create ( FVerticalGroup );
    FlblMaxXValue   := TLabel.Create ( FVerticalGroup );
    FlblInc         := TLabel.Create ( FVerticalGroup );

    FlblMaxYValue   := TLabel.Create ( FHorizontalGroup );
    FlblMinYValue   := TLabel.Create ( FHorizontalGroup );

    FedtMinXValue   := TEdit.Create ( FVerticalGroup );
    FedtMaxXValue   := TEdit.Create ( FVerticalGroup );
    FedtInc         := TEdit.Create ( FVerticalGroup );

    FedtMinYValue   := TEdit.Create ( FHorizontalGroup );
    FedtMaxYValue   := TEdit.Create ( FHorizontalGroup );



    FVerticalGroup.Parent   := Self;
    FHorizontalGroup.Parent := Self;

    FlblMinXValue.Parent := FVerticalGroup;
    FlblMaxXValue.Parent := FVerticalGroup;
    FlblInc.Parent       := FVerticalGroup;

    FlblMinYValue.Parent := FHorizontalGroup;
    FlblMaxYValue.Parent := FHorizontalGroup;


    FedtMinXValue.Parent := FVerticalGroup;
    FedtMaxXValue.Parent := FVerticalGroup;
    FedtInc.Parent       := FVerticalGroup;

    FedtMaxYValue.Parent := FHorizontalGroup;
    FedtMinYValue.Parent := FHorizontalGroup;

    FPnlButtons := TPanel.Create ( Self );
    FBtnOK      := TBitBtn.Create ( Self );
    FBtnCancel  := TBitBtn.Create ( Self );
    FBtnReset   := TBitBtn.Create ( Self );

    FPnlButtons.Parent     := Self;
    FBtnOK.Parent          := FPnlButtons;
    FBtnCancel.Parent      := FPnlButtons;
    FBtnReset.Parent       := FPnlButtons;

    FBtnOK.ModalResult     := mrOK;
    FBtnCancel.ModalResult := mrCancel;
    FOnkeyPress            := nil;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.SetOnClickHandle ( aClickHandle : TClickHandle );
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.SetOnClickHandle';
begin
  try
    FClickHandle := aClickHandle;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorXYAxisReset.GetIncValue : double;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.GetIncValue';
begin
  Result := 0.0;
  try
    Result := StrToFloat ( FedtInc.Text )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorXYAxisReset.GetMaxXValue : double;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.GetMaxXValue';
begin
  Result := 0.0;
  try
    Result := StrToFloat ( FedtMaxXValue.Text )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorXYAxisReset.GetMaxYValue : TDateTime;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.GetMaxYValue';
begin
  Result := 0;
  try
    Result := StrToDate ( FedtMaxYValue.Text )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorXYAxisReset.GetMinXValue : double;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.GetMinXValue';
begin
  Result := 0.0;
  try
    Result := StrToFloat ( FedtMinXValue.Text )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorXYAxisReset.GetMinYValue : TDateTime;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.GetMinYValue';
begin
  Result := 0;
  try
    Result := StrToDate ( FedtMinYValue.Text )
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorXYAxisReset.Initialise : boolean;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.Initialise';
begin
  Result := False;
  try

    Self.BorderStyle := bsDialog;

//    FVerticalGroup.Top   := 5;
//    FVerticalGroup.Left := 10;

//    FVerticalGroup.Width := Self.Width - 20;
//    FVerticalGroup.Height := ClientHeight div 2;

    FVerticalGroup.Align := alTop;
//    FHorizontalGroup.Top := FVerticalGroup.Height + 5;
//    FHorizontalGroup.Left := 10;
//    FHorizontalGroup.Width := FVerticalGroup.Width;
//    FHorizontalGroup.Height := ClientHeight - ( FVerticalGroup.Height +  FPnlButtons.Height );
    FHorizontalGroup.Align := alClient;

    FlblMinXValue.Top := 15;
    FlblMaxXValue.Top := 40;
    FlblInc.Top       := 65;

    FlblMinYValue.Top := 15;
    FlblMaxYValue.Top := 40;



    FlblMinXValue.Left := clblLeftSpace;
    FlblMinYValue.Left := clblLeftSpace;
    FlblMaxXValue.Left := clblLeftSpace;
    FlblMaxYValue.Left := clblLeftSpace;
    FlblInc.Left       := clblLeftSpace;

    FedtMinXValue.Top := FlblMinXValue.Top;
    FedtMinYValue.Top := FlblMinYValue.Top;
    FedtMaxXValue.Top := FlblMaxXValue.Top;
    FedtMaxYValue.Top := FlblMaxYValue.Top;
    FedtInc.Top       := FlblInc.Top;

    FedtMinXValue.Left := FlblMaxYValue.Left + FlblMaxYValue.Width + cEdtSpace;
    FedtMinYValue.Left := FlblMinYValue.Left + FlblMinYValue.Width + cEdtSpace;
    FedtMaxXValue.Left := FlblMaxXValue.Left + FlblMaxXValue.Width + cEdtSpace;
    FedtMaxYValue.Left := FlblMaxYValue.Left + FlblMaxYValue.Width + cEdtSpace;
    FedtInc.Left       := FlblInc.Left + FlblInc.Width + cEdtSpace;

    FedtMinXValue.width := 100;
    FedtMinYValue.width := FedtMinXValue.width;
    FedtMaxXValue.width := FedtMinXValue.width;
    FedtMaxYValue.width := FedtMinXValue.width;
    FedtInc.Width := FedtMinXValue.Width;


    FPnlButtons.Height := 40;
    FBtnOK.Width       := 80;
    FBtnOK.Height      := 30;
    FBtnCancel.Width   := 80;
    FBtnCancel.Height  := 30;
    FBtnReset.Width    := 80;
    FBtnReset. Height  := 30;

    FPnlButtons.BevelInner := bvLowered;
    FPnlButtons.Align     := alBottom;

    FedtMinXValue.Text := cDefaultValue;
    FedtMaxXValue.Text := cDefaultValue;

    FBtnOK.Left     := btnSpace;
    FBtnCancel.Left := FBtnOK.Left + FBtnOK.Width + btnSpace;
    FBtnReset.Left  := FBtnCancel.Left + FBtnCancel.Width + btnSpace;

    FBtnOK.Top      := ( FBtnOK.Parent.Height - FBtnOK.Height ) div 2;
    FBtnCancel.Top  := ( FBtnCancel.Parent.Height - FBtnCancel.Height ) div 2;
    FBtnReset.Top   := ( FBtnReset.Parent.Height - FBtnReset.Height ) div 2;

    FBtnReset.OnClick := DoOnclick;

    FMinXValue   := 0;
    FMinYValue   := 0;
    FMaxXValue   := 0;
    FMaxYValue   := 0;
    FInc         := 0;

    Result := True;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorXYAxisReset.LanguageHasChanged : boolean;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.LanguageHasChanged';
begin
  Result := False;
  try
    FVerticalGroup.Caption := FAppModules.Language.GetString ( 'TSCCaptionXYAxisReset.VerticalGroup' );
    FHorizontalGroup.Caption := FAppModules.Language.GetString ( 'TSCCaptionXYAxisReset.HorizontalGroup' );
    FlblMinXValue.Caption := FAppModules.Language.GetString ( 'TSCCaptionXYAxisReset.MinVertAxis' );
    FlblMinYValue.Caption := FAppModules.Language.GetString ( 'TSCCaptionXYAxisReset.MinHorizAxis' );
    FlblMaxXValue.Caption := FAppModules.Language.GetString ( 'TSCCaptionXYAxisReset.MaxVertAxis' );
    FlblMaxYValue.Caption := FAppModules.Language.GetString ( 'TSCCaptionXYAxisReset.MaxHorizAxis' );
    FlblInc.Caption       := FAppModules.Language.GetString ( 'TSCCaptionXYAxisReset.IncrementVertBy' );

    FPnlButtons.Caption := '';
    Self.Caption := FAppModules.Language.GetString ( 'TSCCaptionXYAxisReset.FormCaption' );
    FBtnOK.Caption := FAppModules.Language.GetString ( 'TSCCaptionSelector.BtnOK_Caption' );
    FBtnCancel.Caption := FAppModules.Language.GetString ( 'TSCCaptionSelector.BtnCancel_Caption' );
    FBtnReset.Caption := FAppModules.Language.GetString ( 'MenuCaption.ViewReset' );

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.Resize;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.SetEnabled(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.SetEnabled';
begin
  try
    //inherited SetEnabled(AEnabled);
    if not AEnabled then
    begin
      FedtMinXValue.Color      := clLtGray;
      FedtMinYValue.Color      := clLtGray;
      FedtMaxXValue.Color      := clLtGray;
      FedtMaxYValue.Color      := clLtGray;
      FedtInc.Color            := clLtGray;
    end
    else
    begin
      FedtMinXValue.Color := clWindow;
      FedtMinYValue.Color := clWindow;
      FedtMaxXValue.Color := clWindow;
      FedtMaxYValue.Color := clWindow;
      FedtInc.Color       := clWindow;
    end;
    FedtMinXValue.ReadOnly := not AEnabled;
    FedtMinYValue.ReadOnly := not AEnabled;
    FedtMaxXValue.ReadOnly := not AEnabled;
    FedtMaxYValue.ReadOnly := not AEnabled;
    FedtInc.ReadOnly       := not AEnabled;
    FBtnOK.Enabled         := AEnabled;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.SetIncValue ( const aValue : double );
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.SetIncValue';
begin
  try
    FedtInc.Text := FloatToStr ( aValue );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.SetMaxXValue ( const aValue : double );
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.SetMaxXValue';
begin
  try
    FedtMaxXValue.Text := FloatToStr ( aValue );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.SetMaxYValue ( const aValue : TDateTime );
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.SetMaxYValue';
begin
  try
    FedtMaxYValue.Text := DateToStr ( aValue );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.SetMinXValue ( const aValue : double );
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.SetMinXValue';
begin
  try
    FedtMinXValue.Text := FloatToStr ( aValue );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.SetMinYValue ( const aValue : TDateTime );
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.SetMinYValue';
begin
  try
    FedtMinYValue.Text := DateToStr ( aValue );
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.SetOnKeyPress(AKeyPressEvent: TKeyPressEvent);
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.SetOnKeyPress';
begin
  try
    if Assigned(AKeyPressEvent) then
    begin
      FedtMinXValue.OnKeyPress := AKeyPressEvent;
      FedtMinYValue.OnKeyPress := AKeyPressEvent;
      FedtMaxXValue.OnKeyPress := AKeyPressEvent;
      FedtMaxYValue.OnKeyPress := AKeyPressEvent;
      FedtInc.OnKeyPress       := AKeyPressEvent;
      FOnkeyPress              := AKeyPressEvent;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorXYAxisReset.StudyHasChanged : boolean;
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorXYAxisReset.DoOnclick ( Sender: TObject );
const OPNAME = 'TTimeSeriesComparitorXYAxisReset.DoOnclick';
begin
  try
    if Assigned ( FClickHandle ) then
      OnClickHandle ( nil, FMinXValue, FMinYValue,
                      FMaxXValue, FMaxYValue, FInc );
    MinXValue := FMinXValue;
    MinYValue := FMinYValue;
    MaxXValue := FMaxXValue;
    MaxYValue := FMaxYValue;
    IncValue  := FInc;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
