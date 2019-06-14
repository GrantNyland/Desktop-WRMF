//
//
//  UNIT      : Contains TSwitchDialog Classes
//  AUTHOR    : Dhlamini Samuel (Cornastone)
//  DATE      : 10/10/2013
//  COPYRIGHT : Copyright © 2015 DWA
//
//
unit USwitchDialog;

interface
uses
  Classes,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  UConstants,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent;

type
  TSwitchDialog = class(TAbstractForm)
  protected
    pnlTop              : TPanel;
    pnlBottom           : TPanel;
    FRelationType       : TRadioGroup;
    btnCancel           : TButton;
    btnOK               : TButton;
    FCaptionStr         : string;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure DoOnclick(Sender : TObject);
    function GetSelectedRelationType : integer;

  public

    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
    property CaptionStr : string read FCaptionStr write FCaptionStr;
    property SelectedRelationType : integer read GetSelectedRelationType;

  end;

implementation

uses
  Math,
  UDataSetType,
  VCL.Graphics,
  SysUtils,
  UErrorHandlingOperations,
  UProgressDialog, DB, VCL.Grids;

{ TImportDiversionDataDialog }

procedure TSwitchDialog.CreateMemberObjects;
const OPNAME = 'TSwitchDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try

    pnlTop                     := TPanel.Create(Self);
    pnlBottom                  := TPanel.Create(Self);

    btnCancel                  := TButton.Create(Self);
    btnOK                      := TButton.Create(Self);
    FRelationType                := TRadioGroup.Create(Self);
    FRelationType.Parent         :=  pnlTop;

    pnlTop.Parent              := Self;
    pnlBottom.Parent           := Self;

    btnCancel.Parent           := pnlBottom;
    btnOK.Parent               := pnlBottom;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDialog.DoOnclick(Sender : TObject);
const OPNAME = 'TSwitchDialog.DoOnclick';
begin
  try
    btnOK.Enabled := (FRelationType.ItemIndex>-1);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDialog.DestroyMemberObjects;
const OPNAME = 'TSwitchDialog.DestroyMemberObjects';
begin
  inherited;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDialog.GetSelectedRelationType : integer;
const OPNAME = 'TSwitchDialog.GetSelectedRelationType';
begin
  Result := -1;
  try
    Result := FRelationType.ItemIndex;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDialog.Initialise: boolean;
const OPNAME = 'TImportDiversionDataDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Self.Color           := clBtnFace;
    Self.Font.Color      := clWindowText;
    Self.Font.Height     := -11;
    Self.Font.Name       := 'MS Sans Serif';
    Self.Font.Style      := [];
    Self.OldCreateOrder  := False;
    Self.Position        := poDefaultPosOnly;
    Self.PixelsPerInch   := 96;
    Self.Height          := 200;
    Self.Width           := 420;
    Self.Position        := poScreenCenter;
    Self.BorderStyle     := bsDialog;

    pnlTop.BevelInner    := bvLowered;
    pnlTop.TabOrder      := 0;

    pnlBottom.BevelInner   := bvLowered;
    pnlBottom.TabOrder     := 1;
    btnCancel.TabOrder     := 9;

    btnOK.Enabled     := False;
    btnOK.TabOrder    := 8;

    btnOK.ModalResult         := mrOK;
    btnCancel.ModalResult     := mrCancel;
    FRelationType.OnClick       := DoOnclick;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSwitchDialog.Resize;
const OPNAME = 'TSwitchDialog.Resize';
begin
  inherited;
  try
    pnlTop.Left          := 0;
    pnlTop.Top           := 0;
    pnlTop.Width         := 164;
    pnlTop.Height        := 120;
    pnlTop.Align         := alTop;

    FRelationType.Align    := alClient;

    pnlBottom.Left         := 0;
    pnlBottom.Top          := 121;
    pnlBottom.Width        := 164;
    pnlBottom.Height       := 44;
    pnlBottom.Align        := alBottom;

    btnCancel.Left         := 280;
    btnCancel.Top          := 10;
    btnCancel.Width        := 75;
    btnCancel.Height       := 25;

    btnOK.Left        := 180;
    btnOK.Top         := 10;
    btnOK.Width       := 75;
    btnOK.Height      := 25;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSwitchDialog.LanguageHasChanged: boolean;
const OPNAME = 'TSwitchDialog.LanguageHasChanged';
begin
  Result := Inherited LanguageHasChanged;
  try
    Self.Caption           := 'Select Switch Type';
    btnCancel.Caption      := 'Cancel';
    btnOK.Caption  := 'OK';
    FRelationType.Items.Add('Time-Related Channel Control');
    FRelationType.Items.Add('Reservoir Level-Related Channel Control');
    FRelationType.ItemIndex := -1;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
