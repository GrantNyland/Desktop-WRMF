//
//
//  UNIT      : Contains TIFRTypeDialog Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 15/06/2005
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UIFRTypeDialog;

interface
uses
  controls,
  Classes,
  Forms,
  Grids,
  ComCtrls,
  ExtCtrls,
  Buttons,
  StdCtrls,
  Chart,
  UAbstractObject,
  VoaimsCom_TLB,
  UAbstractComponent;

type
  TIFRTypeDialog = class(TAbstractForm)
  protected
    FIFRType             : TRadioGroup;
    FPnlButtons          : TPanel;
    FBtnOK               : TBitBtn;
    FBtnCancel           : TBitBtn;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure DoOnclick(Sender : TObject);
    function GetIFRFeatureReferenceFlowType : TIFRFeatureReferenceFlowType;
  public
    procedure Resize; override;
    procedure SetEnabled(AEnabled : boolean);override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function Initialise: boolean; override;
    property IFRType : TIFRFeatureReferenceFlowType read GetIFRFeatureReferenceFlowType;
end;

implementation
uses
  SysUtils,
  Graphics,
  UErrorHandlingOperations;
const
  cbSpace = 5;
  btnSpace  = 8;
{TIFRTypeDialog}

procedure TIFRTypeDialog.AssignHelpContext;
const OPNAME = 'TIFRTypeDialog.AssignHelpContext';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRTypeDialog.CreateMemberObjects;
const OPNAME = 'TIFRTypeDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPnlButtons                   := TPanel.Create(Self);
    FBtnOK                        := TBitBtn.Create(Self);
    FBtnCancel                    := TBitBtn.Create(Self);
    FIFRType                      := TRadioGroup.Create(Self);
    Self.Position                 := poScreenCenter;

    FIFRType.Parent               := Self;
    FPnlButtons.Parent            := Self;
    FBtnOK.Parent                 := FPnlButtons;
    FBtnCancel.Parent             := FPnlButtons;
    FBtnOK.ModalResult            := mrOK;
    FBtnCancel.ModalResult        := mrCancel;
    FBtnOK.OnClick                := DoOnclick;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRTypeDialog.Initialise : boolean;
const OPNAME = 'TIFRTypeDialog.Initialise';
begin
  Result := False;
  try
    Self.BorderStyle           := bsDialog;
    Self.Height                := 130;
    FIFRType.Align             := alClient;
    FPnlButtons.BevelInner     := bvLowered;
    FPnlButtons.Align          := alBottom;
    FPnlButtons.Height         := 40;
    FBtnOK.Width               := 80;
    FBtnOK.Height              := 30;
    FBtnCancel.Width           := 80;
    FBtnCancel.Height          := 30;
    FBtnOK.Left                := btnSpace;
    FBtnCancel.Left            := FBtnOK.Left + FBtnOK.Width + btnSpace;
    FBtnOK.Top                 := (FBtnOK.Parent.Height - FBtnOK.Height) div 2;
    FBtnCancel.Top             := (FBtnCancel.Parent.Height - FBtnCancel.Height) div 2;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIFRTypeDialog.LanguageHasChanged : boolean;
const OPNAME = 'TIFRTypeDialog.LanguageHasChanged';
begin
  Result := False;
  try
    FPnlButtons.Caption          := '';
    Self.Caption                 := FAppModules.Language.GetString('DialogCaption.TIFRTypeDialog');
    FIFRType.Columns             := 2;
    FIFRType.Items.Add('Monthly');
    FIFRType.Items.Add('Annual');
    FIFRType.Caption              := FAppModules.Language.GetString('GroupBoxCaption.IFRTypeGrpBox');
    FBtnOK.Caption                := FAppModules.Language.GetString('TSCChartLegendDialog.BtnOkCaption');
    FBtnCancel.Caption            := FAppModules.Language.GetString('TSCChartLegendDialog.BtnCancelCaption');
    FIFRType.ItemIndex            := 0;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRTypeDialog.Resize;
const OPNAME = 'TIFRTypeDialog.Resize';
begin
  inherited Resize;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRTypeDialog.SetEnabled(AEnabled: boolean);
const OPNAME = 'TIFRTypeDialog.SetEnabled';
begin
  try
    FBtnOK.Enabled               := AEnabled;
      if Assigned(FAppModules) and Assigned(FAppModules.Model()) and
        (FAppModules.Model.ModelName = UAbstractObject.CPreProcessor)  then
      begin
        FBtnOK.Enabled                       := true;
      end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TIFRTypeDialog.StudyHasChanged : boolean;
const OPNAME = 'TIFRTypeDialog.StudyHasChanged';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TIFRTypeDialog.DoOnclick(Sender: TObject );
const OPNAME = 'TIFRTypeDialog.DoOnclick';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


function TIFRTypeDialog.GetIFRFeatureReferenceFlowType: TIFRFeatureReferenceFlowType;
const OPNAME = 'TIFRTypeDialog.GetIFRFeatureReferenceFlowType';
begin
  Result := ifrtMonthly;
  try
    case FIFRType.ItemIndex of
      0 : Result := ifrtMonthly;
      1 : REsult := ifrrftAnnual;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
