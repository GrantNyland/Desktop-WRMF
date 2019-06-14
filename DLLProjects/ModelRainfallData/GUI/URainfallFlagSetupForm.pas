{******************************************************************************}
{*  UNIT      : Contains the class TRainfallFlagSetupForm.                    *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/06/05                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallFlagSetupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, VCL.Grids,VCL.Buttons,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChildToolbar,
  VoaimsCom_TLB,
  VCL.ImgList;

type
  TRainfallFlagSetupForm = class(TAbstractForm)
  private
    FScrollBox          : TScrollBox;
    FPnlButtons         : TAbstractPanel;
    FBtnOK              : TSpeedButton;
    FBtnCancel          : TSpeedButton;
    FPnlClient          : TAbstractPanel;
    FLblChangeList      : TLabel;
    FCbxChangeList      : TAbstractComboBox;
    FLblChange          : TLabel;
    FCbxChange          : TAbstractComboBox;
    FFieldProperty      : TAbstractFieldProperty;
    FLblChangeDescr     : TLabel;
    FEdtChangeDescr     : TAbstractFieldEdit;
    FChangeListID       : integer;
    FSelectedFlag       : string;
    FChangeDescr        : string;
    procedure PopulateControls;
    function InputValid (var AChangeListID : integer;
                         var ASelectedFlag : string;
                         var AChangeDescr  : string): Boolean;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FormClose (Sender     : TObject;
                         var Action : TCloseAction);
    procedure FormShow (Sender: TObject);
    procedure OnCancelClick(Sender: TObject);
    procedure OnOKClick(Sender: TObject);
  public
    function StudyDataHasChanged(AContext: TChangeContext;
                                 AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetData (AChangeListID      : integer;
                       ASelectedFlag      : string;
                       AFieldPropertyName : string);
    property ChangeListID       : integer read FChangeListID;
    property SelectedFlag       : string  read FSelectedFlag;
    property ChangeDescr        : string  read FChangeDescr;
  end;

implementation

uses
  System.UITypes,
  UDataSetType,
  UConstants,
  UDBConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TRainfallFlagSetupForm                                                     *}
{******************************************************************************}

procedure TRainfallFlagSetupForm.CreateMemberObjects;
const OPNAME = 'TRainfallFlagSetupForm.CreateMemberObjects';
begin
  try
    Position := poScreenCenter;
    OnClose  := FormClose;
    OnShow   := FormShow;
    ClientHeight := 150;
    ClientWidth  := 500;

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
      Height     := 100;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;
    FLblChangeList := TLabel.Create(Self);
    with FLblChangeList do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 10;
      Width    := 110;
      Height   := 13;
    end;
    FCbxChangeList := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxChangeList do
    begin
      Parent       := FPnlClient;
      Left         := 120;
      Top          := 10;
      Width        := 180;
      Height       := 21;
      Style        := csDropDownList;
      ItemHeight   := 13;
      TabOrder     := 0;
    end;
    FLblChange := TLabel.Create(Self);
    with FLblChange do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 40;
      Width    := 110;
      Height   := 13;
    end;
    FCbxChange := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxChange do
    begin
      Parent    := FPnlClient;
      Left      := 120;
      Top       := 40;
      Width     := 60;
      Height    := 21;
      TabOrder  := 1;
    end;
    FLblChangeDescr := TLabel.Create(Self);
    with FLblChangeDescr do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 70;
      Width    := 110;
      Height   := 13;
    end;
    FEdtChangeDescr := TAbstractFieldEdit.Create(Self, FAppModules);
    with FEdtChangeDescr do
    begin
      Parent    := FPnlClient;
      Left      := 120;
      Top       := 70;
      Width     := 180;
      Height    := 21;
      TabOrder  := 1;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallFlagSetupForm.Initialise: boolean;
const OPNAME = 'TRainfallFlagSetupForm.Initialise';
begin
  Result := FALSE;
  try
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagSetupForm.FormShow(Sender: TObject);
const OPNAME = 'TRainfallFlagSetupForm.FormShow';
begin
  try
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallFlagSetupForm.DestroyMemberObjects;
const OPNAME = 'TRainfallFlagSetupForm.DestroyMemberObjects';
begin
  try
    FFieldProperty := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallFlagSetupForm.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallFlagSetupForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Caption          := FAppModules.Language.GetString('Rainfall.FlagSetup');
    FBtnOK.Hint      := FAppModules.Language.GetString('ButtonHint.CPOK');
    FBtnCancel.Hint  := FAppModules.Language.GetString('ButtonHint.CPCancel');

    FLblChangeList.Caption  := FAppModules.Language.GetString('ChangeLists.ChangeList') + ' :';
    FLblChange.Caption      := FAppModules.Language.GetString('ChangeLists.Change') + ' :';
    FLblChangeDescr.Caption := FAppModules.Language.GetString('ChangeLists.Description') + ' :';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallFlagSetupForm.StudyDataHasChanged (AContext: TChangeContext;
                                          AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TRainfallFlagSetupForm.StudyDataHasChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagSetupForm.FormClose (Sender     : TObject;
                                          var Action : TCloseAction);
const OPNAME = 'TRainfallFlagSetupForm.FormClose';
begin
  try
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagSetupForm.SetData (AChangeListID      : integer;
                                          ASelectedFlag      : string;
                                          AFieldPropertyName : string);
const OPNAME = 'TRainfallFlagSetupForm.SetData';
begin
  try
    FChangeListID := AChangeListID;
    FSelectedFlag := ASelectedFlag;
    FFieldProperty := nil;
    if (Trim(AFieldPropertyName) <> '') then
      FFieldProperty := FAppModules.FieldProperties.FieldProperty(Trim(AFieldPropertyName));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallFlagSetupForm.PopulateControls;
const OPNAME = 'TRainfallFlagSetupForm.PopulateControls';
var
  lChangeList  : IChangeList;
  lIndex       : integer;
  LUpdateUser  : boolean;
  lListIdx     : integer;
  lFlagIdx     : integer;
begin
  try
    LUpdateUser := (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    LUpdateUser := LUpdateUser and (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));

    FBtnOK.Enabled     := LUpdateUser;
    FBtnCancel.Enabled := LUpdateUser;

    lListIdx := -1;
    FCbxChangeList.Items.Clear;
    for lIndex := 0 to FAppModules.Changes.ChangeLists.Count - 1 do
    begin
      lChangeList := FAppModules.Changes.ChangeListWithIndex(lIndex);
      if (lChangeList.ChangeListID = FChangeListID) then
        lListIdx := lIndex;
      FCbxChangeList.Items.AddObject(lChangeList.ChangeListName, TObject(lChangeList.ChangeListID));
    end;
    if (lListIdx >= 0) then
      FCbxChangeList.ItemIndex := lListIdx
    else
    if (FCbxChangeList.Items.Count > 0) then
      FCbxChangeList.ItemIndex := 0;

    if (FFieldProperty <> nil) then
    begin
      FCbxChange.Clear;
      lFlagIdx := -1;
      if (FFieldProperty.FieldGroup in [fgValidation, fgValidationArray]) then
      begin
        FCbxChange.Items.CommaText := FFieldProperty.FieldAcceptedValues;
        lIndex := FCbxChange.Items.IndexOf('[');
        if (lIndex >= 0) then
          FCbxChange.Items.Delete(lIndex);
        for lIndex := 0 to FCbxChange.Items.Count - 1 do
        begin
          if (FCbxChange.Items[lIndex] = FSelectedFlag) OR
             ((FCbxChange.Items[lIndex] = 'None') AND (FSelectedFlag = '')) then
            lFlagIdx := lIndex;
        end;
      end;
      FCbxChange.ItemIndex := lFlagIdx;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallFlagSetupForm.InputValid (var AChangeListID : integer;
                                            var ASelectedFlag : string;
                                            var AChangeDescr  : string) : Boolean;
const OPNAME = 'TRainfallFlagSetupForm.InputValid';
var
  lIndex : integer;
  lMsg   : string;
begin
  Result := FALSE;
  try
    Result := TRUE;
    lIndex := FCbxChangeList.ItemIndex;
    if (lIndex < 0) then
    begin
      Result := FALSE;
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectChangeList'));
    end
    else
    begin
      AChangeListID := Integer(FCbxChangeList.Items.Objects[lIndex]);
    end;
    if (Result) then
    begin
      ASelectedFlag := FCbxChange.Text;
      AChangeDescr  := FEdtChangeDescr.Text;
      if (ASelectedFlag = 'None') then
      begin
        lMsg := FAppModules.Language.GetString('Rainfall.ConfirmSelectNoFlag');
        if (MessageDlg(lMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
        begin
          Result := FALSE;
          ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectChangeFlag'));
        end;
      end
      else
      if (ASelectedFlag = '') then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectChangeFlag'));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagSetupForm.OnOKClick(Sender: TObject);
const OPNAME = 'TRainfallFlagSetupForm.OnOKClick';
var
  LChangeDescr,
  lSelectedFlag : string;
  lChangeListID : integer;
begin
  try
    if (InputValid(lChangeListID, lSelectedFlag,LChangeDescr)) then
    begin
      FChangeListID := lChangeListID;
      FSelectedFlag := lSelectedFlag;
      FChangeDescr  := LChangeDescr;
      ModalResult   := mrOk;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagSetupForm.OnCancelClick(Sender: TObject);
const OPNAME = 'TRainfallFlagSetupForm.OnCancelClick';
begin
  try
    ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
