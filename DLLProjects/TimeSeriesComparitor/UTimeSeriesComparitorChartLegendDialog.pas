//
//
//  UNIT      : Contains TimeSeriesComparitorChartLegendDialog Class
//  AUTHOR    : Christopher Levendall(Cornastone)
//  DATE      : 15/06/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UTimeSeriesComparitorChartLegendDialog;

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
  TTimeSeriesComparitorChartLegendDialog = class ( TAbstractForm )
  protected
    FPnlButtons          : TPanel;

    FBtnOK               : TBitBtn;
    FBtnCancel           : TBitBtn;

    FLegendPositionGroup : TRadioGroup;

    FLegendAlignment     : TLegendAlignment;
    FLegendVisible       : boolean;
    FlblSeriesName       : TLabel;
    FedtSeriesName       : TEdit;
    function GetLegendAlignment : TLegendAlignment;
    function GetLegendVisible : boolean;
    procedure SetLegendAlignment(ALegendAlignment : TLegendAlignment);
    procedure SetLegendVisible(ALegendVisible : boolean);
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure DoOnclick ( Sender : TObject );
    procedure SetSeriesName ( const Value: string);
    function GetSeriesName : string;

  public
    procedure Resize; override;
    procedure SetEnabled(AEnabled : boolean);override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function Initialise: boolean; override;
    property LegendAlignment  : TLegendAlignment read GetLegendAlignment write SetLegendAlignment;
    property LegendVisible   : boolean read GetLegendVisible write SetLegendVisible;
    property SeriesName      : string read GetSeriesName write SetSeriesName;
end;

implementation
uses
  SysUtils,
  VCL.Graphics,
  UTimeSeriesComparitorLinkClasses,
  UErrorHandlingOperations;
const
  cbSpace = 5;
{TimeSeriesComparitorChartLegendDialog}

procedure TTimeSeriesComparitorChartLegendDialog.AssignHelpContext;
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.AssignHelpContext';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartLegendDialog.CreateMemberObjects;
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FLegendPositionGroup          := TRadioGroup.Create (Self);

    FLegendPositionGroup.Parent   := Self;
    FlblSeriesName                := TLabel.Create(Self);
    FedtSeriesName                := TEdit.Create(Self);
    FPnlButtons                   := TPanel.Create ( Self );

    FBtnOK                        := TBitBtn.Create ( Self );
    FBtnCancel                    := TBitBtn.Create ( Self );
    FlblSeriesName.Parent         := Self;
    FedtSeriesName.Parent         := Self;
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

function TTimeSeriesComparitorChartLegendDialog.Initialise : boolean;
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.Initialise';
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
    FLegendPositionGroup.Height  := ClientHeight - FPnlButtons.Height - FlblSeriesName.Height - 10;

    FlblSeriesName.Top         := FLegendPositionGroup.Height + 5;
    FlblSeriesName.Left        := FLegendPositionGroup.Left;
    FlblSeriesName.Caption     := FAppModules.Language.GetString ( 'TSCCaptionSelectorPanel.Series');

    FedtSeriesName.Top         := FlblSeriesName.Top;
    FedtSeriesName.Left        := FlblSeriesName.Width + 10;
    FedtSeriesName.Width       := FLegendPositionGroup.Width - ( FlblSeriesName.Width + 10 );
//    FLegendPositionGroup.Align := alTop;

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

function TTimeSeriesComparitorChartLegendDialog.LanguageHasChanged : boolean;
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.LanguageHasChanged';
begin
  Result := False;
  try
    FLegendPositionGroup.Caption := FAppModules.Language.GetString ( 'TSCChartLegendDialog.GroupCaption' );

    FPnlButtons.Caption          := '';
    Self.Caption                 := FAppModules.Language.GetString ( 'TSCChartLegendDialog.DialogCaption' );
    FBtnOK.Caption               := FAppModules.Language.GetString ( 'TSCChartLegendDialog.BtnOkCaption' );
    FBtnCancel.Caption           := FAppModules.Language.GetString ( 'TSCChartLegendDialog.BtnCancelCaption' );
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartLegendDialog.Resize;
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorChartLegendDialog.GetLegendAlignment: TLegendAlignment;
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.GetLegendAlignment';
begin
  Result := laLeft;
  try
    Result := FLegendAlignment;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorChartLegendDialog.GetLegendVisible: boolean;
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.GetLegendVisible';
begin
  Result := False;
  try
    Result := FLegendVisible;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartLegendDialog.SetEnabled(AEnabled: boolean);
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.SetEnabled';
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

function TTimeSeriesComparitorChartLegendDialog.StudyHasChanged : boolean;
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartLegendDialog.DoOnclick(Sender: TObject );
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.DoOnclick';
begin
  try
    FLegendAlignment := TLegendAlignment(FLegendPositionGroup.ItemIndex - 1);
    LegendVisible    := (FLegendPositionGroup.ItemIndex > 0);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartLegendDialog.SetLegendAlignment(ALegendAlignment: TLegendAlignment);
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.SetLegendAlignment';
begin
  try
    FLegendAlignment := ALegendAlignment;
    FLegendPositionGroup.ItemIndex := Integer(FLegendAlignment) + 1;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartLegendDialog.SetLegendVisible(ALegendVisible: boolean);
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.SetLegendVisible';
begin
  try
    FLegendVisible := ALegendVisible;
    if not FLegendVisible then
      FLegendPositionGroup.ItemIndex := 0;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TTimeSeriesComparitorChartLegendDialog.SetSeriesName ( const Value : string );
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.SetSeriesName';
begin
  try
    FedtSeriesName.Text := Value;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TTimeSeriesComparitorChartLegendDialog.GetSeriesName: string;
const OPNAME = 'TTimeSeriesComparitorChartLegendDialog.GetSeriesName';
begin
  Result := '';
  try
    Result := FedtSeriesName.Text;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
