//
//
//  UNIT      : Contains TOutputComparitorChartLegendDialog Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 15/06/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UOutputComparitorChartLegendDialog;

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
  VCLTee.Chart,
  UAbstractObject,
  UAbstractComponent;

type
  TOutputComparitorChartLegendDialog = class(TAbstractForm)
  protected
    FPnlButtons          : TPanel;
    FBtnOK               : TBitBtn;
    FBtnCancel           : TBitBtn;
    FLegendPositionGroup : TRadioGroup;
    FLegendAlignment     : TLegendAlignment;
    FLegendVisible       : boolean;
    function GetLegendAlignment : TLegendAlignment;
    function GetLegendVisible : boolean;
    procedure SetLegendAlignment(ALegendAlignment : TLegendAlignment);
    procedure SetLegendVisible(ALegendVisible : boolean);
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure DoOnclick(Sender : TObject);

  public
    procedure Resize; override;
    procedure SetEnabled(AEnabled : boolean);override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function Initialise: boolean; override;
    property LegendAlignment  : TLegendAlignment read GetLegendAlignment write SetLegendAlignment;
    property LegendVisible   : boolean read GetLegendVisible write SetLegendVisible;
end;

implementation
uses
  SysUtils,
  VCL.Graphics,
  UErrorHandlingOperations;
const
  cbSpace = 5;
  pnlHeight = 30;
  pnlSpace  = 2;
  btnSpace  = 8;

{TOutputComparitorChartLegendDialog}

procedure TOutputComparitorChartLegendDialog.AssignHelpContext;
const OPNAME = 'TOutputComparitorChartLegendDialog.AssignHelpContext';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparitorChartLegendDialog.CreateMemberObjects;
const OPNAME = 'TOutputComparitorChartLegendDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FLegendPositionGroup          := TRadioGroup.Create (Self);
    FLegendPositionGroup.Parent   := Self;
    FPnlButtons                   := TPanel.Create(Self);
    FBtnOK                        := TBitBtn.Create(Self);
    FBtnCancel                    := TBitBtn.Create(Self);
    FPnlButtons.Parent            := Self;
    FBtnOK.Parent                 := FPnlButtons;
    FBtnCancel.Parent             := FPnlButtons;
    FBtnOK.ModalResult            := mrOK;
    FBtnCancel.ModalResult        := mrCancel;
    FBtnOK.OnClick                := DoOnclick;
    FLegendAlignment              := laLeft;
    FLegendVisible                := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparitorChartLegendDialog.Initialise : boolean;
const OPNAME = 'TOutputComparitorChartLegendDialog.Initialise';
begin
  Result := False;
  try

    Self.BorderStyle           := bsDialog;

    FPnlButtons.BevelInner     := bvLowered;
    FPnlButtons.Align          := alBottom;
    FPnlButtons.Height         := 40;

    FLegendPositionGroup.Top     := 5;
    FLegendPositionGroup.Left    := 5;
    FLegendPositionGroup.Width   := ClientWidth -1;
    FLegendPositionGroup.Height  := ClientHeight - FPnlButtons.Height;
    FBtnOK.Width               := 80;
    FBtnOK.Height              := 30;
    FBtnCancel.Width           := 80;
    FBtnCancel.Height          := 30;

    FBtnOK.Left                := btnSpace;
    FBtnCancel.Left            := FBtnOK.Left + FBtnOK.Width + btnSpace;

    FBtnOK.Top                 := (FBtnOK.Parent.Height - FBtnOK.Height) div 2;
    FBtnCancel.Top             := (FBtnCancel.Parent.Height - FBtnCancel.Height) div 2;

    FLegendPositionGroup.Items.Clear;
    FLegendPositionGroup.Items.Add(FAppModules.Language.GetString('TSCChartLegendDialog.LegendNone' ));
    FLegendPositionGroup.Items.Add(FAppModules.Language.GetString('TSCChartLegendDialog.LegendLeft' ));
    FLegendPositionGroup.Items.Add(FAppModules.Language.GetString('TSCChartLegendDialog.LegendRight' ));
    FLegendPositionGroup.Items.Add(FAppModules.Language.GetString('TSCChartLegendDialog.LegendTop' ));
    FLegendPositionGroup.Items.Add(FAppModules.Language.GetString('TSCChartLegendDialog.LegendBottom' ));
    FLegendPositionGroup.ItemIndex := 0;
    FLegendAlignment               := laLeft;
    FLegendVisible                 := False;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparitorChartLegendDialog.LanguageHasChanged : boolean;
const OPNAME = 'TOutputComparitorChartLegendDialog.LanguageHasChanged';
begin
  Result := False;
  try
    FLegendPositionGroup.Caption := FAppModules.Language.GetString('TSCChartLegendDialog.GroupCaption');

    FPnlButtons.Caption          := '';
    Self.Caption                 := FAppModules.Language.GetString('TSCChartLegendDialog.DialogCaption');
    FBtnOK.Caption               := FAppModules.Language.GetString('TSCChartLegendDialog.BtnOkCaption');
    FBtnCancel.Caption           := FAppModules.Language.GetString('TSCChartLegendDialog.BtnCancelCaption');
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparitorChartLegendDialog.Resize;
const OPNAME = 'TOutputComparitorChartLegendDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparitorChartLegendDialog.GetLegendAlignment: TLegendAlignment;
const OPNAME = 'TOutputComparitorChartLegendDialog.GetLegendAlignment';
begin
  Result := laLeft;
  try
    Result := FLegendAlignment;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparitorChartLegendDialog.GetLegendVisible: boolean;
const OPNAME = 'TOutputComparitorChartLegendDialog.GetLegendVisible';
begin
  Result := False;
  try
    Result := FLegendVisible;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparitorChartLegendDialog.SetEnabled(AEnabled: boolean);
const OPNAME = 'TOutputComparitorChartLegendDialog.SetEnabled';
begin
  try
    //inherited SetEnabled(AEnabled);
    FLegendPositionGroup.Enabled := AEnabled;
    FBtnOK.Enabled               := AEnabled;
      if Assigned(FAppModules) and Assigned(FAppModules.Model()) and
        (FAppModules.Model.ModelName = UAbstractObject.CPreProcessor)  then
      begin
        FBtnOK.Enabled                       := true;
        FLegendPositionGroup.Enabled         := true;
      end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparitorChartLegendDialog.StudyHasChanged : boolean;
const OPNAME = 'TOutputComparitorChartLegendDialog.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparitorChartLegendDialog.DoOnclick(Sender: TObject );
const OPNAME = 'TOutputComparitorChartLegendDialog.DoOnclick';
begin
  try
    FLegendAlignment := TLegendAlignment(FLegendPositionGroup.ItemIndex - 1);
    LegendVisible    := (FLegendPositionGroup.ItemIndex > 0);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparitorChartLegendDialog.SetLegendAlignment(ALegendAlignment: TLegendAlignment);
const OPNAME = 'TOutputComparitorChartLegendDialog.SetLegendAlignment';
begin
  try
    FLegendAlignment := ALegendAlignment;
    FLegendPositionGroup.ItemIndex := Integer(FLegendAlignment) + 1;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparitorChartLegendDialog.SetLegendVisible(ALegendVisible: boolean);
const OPNAME = 'TOutputComparitorChartLegendDialog.SetLegendVisible';
begin
  try
    FLegendVisible := ALegendVisible;
    if not FLegendVisible then
      FLegendPositionGroup.ItemIndex := 0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
