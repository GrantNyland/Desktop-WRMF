//
//
//  UNIT      : Contains TStartingStorageCalculate Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 19/09/2005
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UStartingStorageCalculate;

interface
uses
  Classes,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.CheckLst,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent;

type
  TStartingStorageCalculate = class(TAbstractForm)
  protected
    FlblFullSupply          : TLabel;
    FedtFullSupply          : TEdit;
    FlblOldStartingStorage  : TLabel;
    FedtOldStartingStorage  : TEdit;
    FlblNewStartingStorage  : TLabel;
    FedtNewStartingStorage  : TEdit;
    FlblPercentage          : TLabel;
    FedtPercentage          : TEdit;
    FbtnCalculate           : TButton;
    FbtnOK                  : TButton;
    FbtnCancel              : TButton;
    FStartSize              : Boolean;
    procedure btnOKClick(Sender: TObject);
    procedure CreateMemberObjects; override;
  public
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
    procedure Populate(AFullSupplyLevel,AStartingStorageLevel,APercentage: double);
    function Initialise: boolean; override;
    property edtNewStartingStorage :TEdit   read FedtNewStartingStorage;
    property edtOldStartingStorage :TEdit   read FedtOldStartingStorage;
    property edtPercentage         :TEdit   read FedtPercentage;
    property btnCalculate          :TButton read FbtnCalculate;
    property  btnOK                :TButton read FbtnOK;
  end;

implementation

uses
  Math,
  VCL.Graphics,
  SysUtils,
  UErrorHandlingOperations;

{ TStartingStorageCalculate }

procedure TStartingStorageCalculate.CreateMemberObjects;
const OPNAME = 'TStartingStorageCalculate.CreateMemberObjects';
begin
  inherited;
  try
    Self.ActiveControl      := FedtPercentage;
    //Self.BorderIcons        := [];
    FStartSize              := True;

    FlblFullSupply          := TLabel.Create(Self);
    FedtFullSupply          := TEdit.Create(Self);
    FlblOldStartingStorage  := TLabel.Create(Self);
    FedtOldStartingStorage  := TEdit.Create(Self);
    FlblNewStartingStorage  := TLabel.Create(Self);
    FedtNewStartingStorage  := TEdit.Create(Self);
    FlblPercentage          := TLabel.Create(Self);
    FedtPercentage          := TEdit.Create(Self);
    FbtnCalculate           := TButton.Create(Self);
    FbtnOK                  := TButton.Create(Self);
    FbtnCancel              := TButton.Create(Self);

    FlblFullSupply.Parent         := Self;
    FedtFullSupply.Parent         := Self;
    FlblOldStartingStorage.Parent := Self;
    FedtOldStartingStorage.Parent := Self;
    FlblNewStartingStorage.Parent := Self;
    FedtNewStartingStorage.Parent := Self;
    FlblPercentage.Parent         := Self;
    FedtPercentage.Parent         := Self;
    FbtnCalculate.Parent          := Self;
    FbtnOK.Parent                 := Self;
    FbtnCancel.Parent             := Self;

    FedtFullSupply.Color := clInactiveBorder;
    FedtFullSupply.ReadOnly := True;
    FedtOldStartingStorage.Color := clInactiveBorder;
    FedtOldStartingStorage.ReadOnly := True;
    FedtNewStartingStorage.Color := clInactiveBorder;
    FedtNewStartingStorage.ReadOnly := True;

    FbtnOK.ModalResult := 1;
    FbtnCancel.ModalResult := 2;

    FbtnOK.Enabled := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStartingStorageCalculate.Initialise: boolean;
const OPNAME = 'TStartingStorageCalculate.Initialise';
begin
  Result := inherited Initialise;
  try
    FedtFullSupply.Text         := '';
    FedtOldStartingStorage.Text := '';
    FedtNewStartingStorage.Text := '';
    FedtPercentage.Text         := '';
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStartingStorageCalculate.Resize;
const OPNAME = 'TStartingStorageCalculate.Resize';
begin
  inherited;
  try
    if FStartSize then
    begin
      Self.Width         := 380;
      Self.Height        := 220;

      FlblFullSupply.Left   := 8;
      FlblFullSupply.Top    := 16;
      FlblFullSupply.Width  := 220;
      FlblFullSupply.Height := 13;

      FlblOldStartingStorage.Left   := 8;
      FlblOldStartingStorage.Top    := 46;
      FlblOldStartingStorage.Width  := 220;
      FlblOldStartingStorage.Height := 13;

      FlblNewStartingStorage.Left   := 8;
      FlblNewStartingStorage.Top    := 76;
      FlblNewStartingStorage.Width  := 220;
      FlblNewStartingStorage.Height := 13;

      FlblPercentage.Left   := 8;
      FlblPercentage.Top    := 106;
      FlblPercentage.Width  := 220;
      FlblPercentage.Height := 13;

      FedtFullSupply.Left   := 230;
      FedtFullSupply.Top    := 16;
      FedtFullSupply.Width  := 66;
      FedtFullSupply.Height := 21;

      FedtOldStartingStorage.Left   := 230;
      FedtOldStartingStorage.Top    := 44;
      FedtOldStartingStorage.Width  := 65;
      FedtOldStartingStorage.Height := 21;

      FedtNewStartingStorage.Left   := 230;
      FedtNewStartingStorage.Top    := 73;
      FedtNewStartingStorage.Width  := 65;
      FedtNewStartingStorage.Height := 21;

      FedtPercentage.Left   := 230;
      FedtPercentage.Top    := 106;
      FedtPercentage.Width  := 65;
      FedtPercentage.Height := 21;

      FbtnCalculate.Left   := 297;
      FbtnCalculate.Top    := 106;
      FbtnCalculate.Width  := 57;
      FbtnCalculate.Height := 21;

      FbtnOK.Left   := 50;
      FbtnOK.Top    := 144;
      FbtnOK.Width  := 75;
      FbtnOK.Height := 25;

      FbtnCancel.Left   := 168;
      FbtnCancel.Top    := 144;
      FbtnCancel.Width  := 75;
      FbtnCancel.Height := 25;
      FStartSize        := False;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TStartingStorageCalculate.LanguageHasChanged: boolean;
const OPNAME = 'TStartingStorageCalculate.LanguageHasChanged';
begin
  Result := Inherited LanguageHasChanged;
  try
    Self.Caption                   := FAppModules.Language.GetString('DialogCaption.StartingStorageCalculate');
    FlblFullSupply.Caption         := FAppModules.Language.GetString('LabelCaption.FullSupplyDescr');
    FlblOldStartingStorage.Caption := FAppModules.Language.GetString('LabelCaption.OldStartingStorageDescr');
    FlblPercentage.Caption         := FAppModules.Language.GetString('LabelCaption.PercentageDescr');
    FlblNewStartingStorage.Caption := FAppModules.Language.GetString('LabelCaption.NewStartingStorageDescr');
    FbtnCalculate.Caption          := FAppModules.Language.GetString('ButtonCaption.Calculate');
    FbtnOK.Caption                 := FAppModules.Language.GetString('ButtonCaption.OK');
    FbtnCancel.Caption             := FAppModules.Language.GetString('ButtonCaption.Cancel');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStartingStorageCalculate.btnOKClick(Sender: TObject);
const OPNAME = 'TStartingStorageCalculate.btnOKClick';
begin
  try
    Self.ModalResult := TButton(Sender).ModalResult;
    Self.Close;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TStartingStorageCalculate.Populate(AFullSupplyLevel,AStartingStorageLevel,APercentage: double);
const OPNAME = 'TStartingStorageCalculate.Populate';
begin
  try
    FedtFullSupply.Text         := FormatFloat('##0.00',AFullSupplyLevel);
    FedtOldStartingStorage.Text := FormatFloat('##0.00',AStartingStorageLevel);
    FedtNewStartingStorage.Text := FormatFloat('##0.00',AStartingStorageLevel);
    FedtPercentage.Text         := FormatFloat('##0.00',APercentage);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
