unit UPatchParametersDialog;

interface

uses
  Windows, Messages, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.Buttons,VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent;

  type
    TPatchParameterDialog = class(TAbstractForm)
    private
      FPatchValueLabel      : TLabel;
      FSourceValueLabel     : TLabel;
      FStationNoLabel       : TLabel;
      FStationNameLabel     : TLabel;
      FPatchValueEdit       : TFieldEdit;
      FSourceEdit           : TFieldEdit;
      FOKButton             : TFieldButton;
      FCancelButton         : TFieldButton;
      FrgrpPatchSourceValues: TAbstractRadioGroup;
    protected
      procedure CreateMemberObjects; override;

  public
    procedure AfterConstruction; override;
    function LanguageHasChanged: boolean; override;

    property OKBtn              : TFieldButton     read FOKButton;
    property CancelBtn          : TFieldButton     read FCancelButton;
    property lblPatchValue      : TLabel           read FPatchValueLabel;
    property lblSourceValue     : TLabel           read FSourceValueLabel;
    property lblStationNo       : TLabel           read FStationNoLabel;
    property lblStationNameValue: TLabel           read FStationNameLabel;
    property edtSource          : TFieldEdit       read FSourceEdit;
    property edtPatchValue      : TFieldEdit       read FPatchValueEdit;
    property rgPatchSourceValues: TAbstractRadioGroup read FrgrpPatchSourceValues;
  end;

implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{ TPatchParameterDialog }

procedure TPatchParameterDialog.AfterConstruction;
const OPNAME = 'TPatchParameterDialog.AfterConstruction';
begin
  try
    try

      if Assigned(FAppModules) and Assigned(FAppModules.ViewIni()) then
        FAppModules.ViewIni.SaveFormView(self);

    finally
      inherited;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TPatchParameterDialog.CreateMemberObjects;
const OPNAME = 'TPatchParameterDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    Self.AutoSize                := False;
    Self.Height                  := 273;
    self.ClientWidth             := 283;
    self.BorderStyle             := bsDialog;
    self.Position                := poScreenCenter;

    FStationNameLabel            := CreateFieldLabel(self,self,50,16,110,20);
    FStationNameLabel.Font.Style := [fsBold];
    FStationNameLabel.Font.Name  := 'MS Sans Serif';
    FStationNameLabel.Font.Height:= -16;
    FStationNameLabel.Font.Size  := 12;

    FStationNoLabel              := CreateFieldLabel(Self,self,170,16,99,20);
    FStationNoLabel.Font.Style   := [fsBold];
    FStationNoLabel.Font.Name    := 'MS Sans Serif';
    FStationNoLabel.Font.Height  := -16;
    FStationNoLabel.Font.Size    := 12;


    FPatchValueLabel             := CreateFieldLabel(Self,Self,55,64,72,16);
    FPatchValueLabel.Font.Name   := 'MS Sans Serif';
    FPatchValueLabel.Font.Height := -13;
    FPatchValueLabel.Font.Size   := 10;

    FSourceValueLabel            := CreateFieldLabel(Self,Self,167,64,81,16);
    FSourceValueLabel.Font.Name  := 'MS Sans Serif';
    FSourceValueLabel.Font.Height:= -13;
    FSourceValueLabel.Font.Size  := 10;

    FPatchValueEdit              := CreateFieldEdit(FAppModules,self,self,50,88,89,21,0,True);
    FPatchValueEdit.Enabled      := False;
    FSourceEdit                  := CreateFieldEdit(FAppModules,self,self,162,88,105,21,1,True);
    FSourceEdit.Enabled          := False;

   //    FrgrpPatchSourceValues := CreateFieldRadioGroup(FAppModules,Self,Self,50,120,217,41,3,True);
    FrgrpPatchSourceValues := TAbstractRadioGroup.Create(Nil, FAppModules);
    FrgrpPatchSourceValues.Parent := self;
    FrgrpPatchSourceValues.Left := 50;
    FrgrpPatchSourceValues.Top := 120;
    FrgrpPatchSourceValues.Width := 217;
    FrgrpPatchSourceValues.Height := 41;
    FrgrpPatchSourceValues.TabOrder := 3;
    FrgrpPatchSourceValues.Columns := 2;
    FrgrpPatchSourceValues.Items.Add('Patch');
    FrgrpPatchSourceValues.Items.Add('Source');

    FOKButton                    := CreateFieldButton(FAppModules,self,Self,58,178,75,25,2,True,'OK');
    FOKButton.ModalResult        := mrOk;

    FCancelButton                := CreateFieldButton(FAppModules,self,Self,154,178,75,25,2,True,'Cancel');
    FCancelButton.ModalResult    := mrCancel;



  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TPatchParameterDialog.LanguageHasChanged: boolean;
const OPNAME = 'TPatchParameterDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
      FPatchValueLabel.Caption  := FAppModules.Language.GetString('LabelText.PatchValue');
      FSourceValueLabel.Caption := FAppModules.Language.GetString('LabelText.SourceValue');
      FCancelButton.Caption     := FAppModules.Language.GetString('ButtonCaption.Cancel');
      FOKButton.Caption         := FAppModules.Language.GetString('ButtonCaption.OK');
      FStationNameLabel.Caption := FAppModules.Language.GetString('LabelText.StationName');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
