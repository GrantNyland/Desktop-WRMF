unit UUpdateSplitForm;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, VCL.Grids,VCL.Buttons,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChildToolbar,
  VCL.ImgList;

type
  TUpdateSplitForm = class(TAbstractForm)
  protected
    FStationID         : integer;
    FNewStartYear         : integer;
    FNewEndYear           : integer;
    FOldStartYear         : integer;
    FOldEndYear           : integer;

    FScrollBox          : TScrollBox;
    FPnlButtons         : TAbstractPanel;
    FBtnOK              : TSpeedButton;
    FBtnCancel          : TSpeedButton;
    FPnlClient          : TAbstractPanel;
    FLblStationName     : TLabel;
    FLblStationNameVal  : TLabel;
    FLblStartDate       : TLabel;
    FCbxStartYear       : TAbstractComboBox;
    FLblEndDate         : TLabel;
    FCbxEndYear         : TAbstractComboBox;

    FLblOldStartDate       : TLabel;
    FCbxOldStartYear       : TAbstractComboBox;
    FLblOldEndDate         : TLabel;
    FCbxOldEndYear         : TAbstractComboBox;

    procedure CreateMemberObjects;override;
    procedure OnOKClick(Sender: TObject);
    procedure OnCancelClick(Sender: TObject);
    procedure OnFormShow(Sender : TObject);

  public

    function Initialise: boolean;override;
    property NewStartYear : integer read FNewStartYear write FNewStartYear;
    property NewEndYear   : integer read FNewEndYear write FNewEndYear;
    property OldStartYear : integer read FOldStartYear write FOldStartYear;
    property OldEndYear : integer read FOldEndYear write FOldEndYear;
    function LanguageHasChanged: Boolean; override;


end;

implementation
uses
  UConstants,
  UErrorHandlingOperations;


procedure TUpdateSplitForm.CreateMemberObjects;
const OPNAME = 'TUpdateSplitForm.CreateMemberObjects';
begin
  try
    FStationID     := 0;
    FNewStartYear  := 0;
    FNewEndYear    := 0;
    FOldStartYear  := 0;
    FOldEndYear    := 0;

    Position := poScreenCenter;
    OnShow   := OnFormShow;
    ClientHeight := 60;
    ClientWidth  := 250;

    FScrollBox := TScrollBox.Create(Self);
    with FScrollBox do
    begin
      Parent     := Self;
      Left       := 0;
      Top        := 0;
      Align      := alClient;
      BevelInner := bvNone;
      TabOrder   := 0;
    end;
    FPnlButtons := TAbstractPanel.Create(Self, FAppModules);
    with FPnlButtons do
    begin
      Parent     := FScrollBox;
      Left       := 0;
      Top        := 0;
      Height     := 30;
      Align      := alTop;
      BevelOuter := bvNone;
      TabOrder   := 0;
    end;
    FBtnOK := TSpeedButton.Create(FPnlButtons);
    FBtnOK.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPOK'));
    with FBtnOK do
    begin
      Parent   := FPnlButtons;
      Left     := 0;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 0;
      ShowHint := TRUE;
      OnClick  := OnOKClick;
    end;
    FBtnCancel := TSpeedButton.Create(FPnlButtons);
    FBtnCancel.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPCancel'));
    with FBtnCancel do
    begin
      Parent   := FPnlButtons;
      Left     := 30;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 1;
      ShowHint := TRUE;
      OnClick  := OnCancelClick;
    end;
    FPnlClient := TAbstractPanel.Create(Self, FAppModules);
    with FPnlClient do
    begin
      Parent     := FScrollBox;
      Left       := 0;
      Top        := 30;
      Width      := 300;
      Height     := 200;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;
    FLblStationName := TLabel.Create(Self);
    with FLblStationName do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 10;
      Width    := 130;
      Height   := 13;
    end;
    FLblStationNameVal := TLabel.Create(Self);
    with FLblStationNameVal do
    begin
      Parent   := FPnlClient;
      AutoSize := TRUE;
      Left     := 140;
      Top      := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
    FLblStartDate := TLabel.Create(Self);
    with FLblStartDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 40;
      Width    := 140;
      Height   := 13;
    end;
    FCbxStartYear := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxStartYear do
    begin
      Parent   := FPnlClient;
      Left     := 140;
      Top      := 40;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 2;
    end;
    FLblEndDate := TLabel.Create(Self);
    with FLblEndDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 70;
      Width    := 130;
      Height   := 13;
    end;
    FCbxEndYear := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxEndYear do
    begin
      Parent   := FPnlClient;
      Left     := 140;
      Top      := 70;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 4;
    end;

    FLblOldStartDate := TLabel.Create(Self);
    with FLblOldStartDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 100;
      Width    := 130;
      Height   := 13;
      Enabled  := False;
    end;

    FCbxOldStartYear := TAbstractComboBox.Create(Self,FAppModules);
    with FCbxOldStartYear do
    begin
      Parent   := FPnlClient;
      Left     := 140;
      Top      := 100;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 4;
    end;

    FLblOldEndDate := TLabel.Create(Self);
    with FLblOldEndDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 130;
      Width    := 130;
      Height   := 13;
      Enabled  := False;
    end;

    FCbxOldEndYear := TAbstractComboBox.Create(Self,FAppModules);
    with FCbxOldEndYear do
    begin
      Parent   := FPnlClient;
      Left     := 140;
      Top      := 130;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 4;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TUpdateSplitForm.Initialise: boolean;
const OPNAME = 'TUpdateSplitForm.Initialise';
begin
  Result := False;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUpdateSplitForm.OnOKClick(Sender: TObject);
const OPNAME = 'TUpdateSplitForm.OnOKClick';
begin
  try
    OldStartYear := StrToInt(FCbxOldStartYear.Text);
    OldEndYear :=  StrToInt(FCbxOldEndYear.Text);
    NewStartYear := StrToInt(FCbxStartYear.Text);
    NewEndYear := StrToInt(FCbxEndYear.Text);
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TUpdateSplitForm.OnCancelClick(Sender: TObject);
const OPNAME = 'TUpdateSplitForm.OnCancelClick';
begin
  try
    ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TUpdateSplitForm.LanguageHasChanged: Boolean;
const OPNAME = 'TUpdateSplitForm.LanguageHasChanged';
begin
  Result := False;
  try
    FLblOldStartDate.Caption := FAppModules.Language.GetString('LabelText.StationStartDate');
    FLblOldEndDate.Caption   := FAppModules.Language.GetString('LabelText.StationEndDate');
    FLblStartDate.Caption    := FAppModules.Language.GetString('LabelText.SplitStartDate');
    FLblEndDate.Caption      := FAppModules.Language.GetString('LabelText.SplitEndDate');
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TUpdateSplitForm.OnFormShow(Sender : TObject);
const OPNAME = 'TUpdateSplitForm.OnFormShow';
var
  LIndex : integer;
begin
  try
    for LIndex := FNewStartYear to FNewEndYear do
    begin
      FCbxStartYear.Items.Add(IntToStr(LIndex));
      FCbxEndYear.Items.Add(IntToStr(LIndex));
      FCbxOldStartYear.Items.Add(IntToStr(LIndex));
      FCbxOldEndYear.Items.Add(IntToStr(LIndex));
    end;
    FCbxStartYear.ItemIndex := FCbxStartYear.Items.IndexOf(IntToStr(FOldStartYear));
    FCbxEndYear.ItemIndex   := FCbxEndYear.Items.IndexOf(IntToStr(FOldEndYear));
    FCbxOldStartYear.Enabled := False;
    FCbxOldStartYear.ItemIndex   := FCbxOldStartYear.Items.IndexOf(IntToStr(FNewStartYear));
    FCbxOldEndYear.Enabled := False;
    FCbxOldEndYear.ItemIndex     := FCbxOldEndYear.Items.IndexOf(IntToStr(FNewEndYear));
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


end.
