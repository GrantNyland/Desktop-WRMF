{******************************************************************************}
{*  UNIT      : Contains the class TSelectRawFlagsForm.                       *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/06/05                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit USelectRawFlagsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, VCL.Grids,VCL.Buttons,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChildToolbar,
  RainfallCom_TLB,
  VCL.ImgList;

type
  TSelectRawFlagsForm = class(TAbstractForm)
  private
    FParamField        : string;
    FFieldProperty     : TAbstractFieldProperty;
    FStationID         : integer;
    FPatchID           : integer;

    FScrollBox          : TScrollBox;
    FPnlButtons         : TAbstractPanel;
    FBtnOK              : TSpeedButton;
    FBtnCancel          : TSpeedButton;
    FPnlClient          : TAbstractPanel;
    FLblSelectRAWFlags  : TLabel;
    FClbSelectRAWFlags  : TFieldCheckListBox;

    procedure PopulateControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FormClose (Sender     : TObject;
                         var Action : TCloseAction);
    procedure FormShow (Sender: TObject);
    procedure OnResizeForm (Sender : TObject);
    procedure OnCancelClick(Sender: TObject);
    procedure OnOKClick(Sender: TObject);
  public
    function StudyDataHasChanged(AContext: TChangeContext;
                                 AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetData (AParamField : string;
                       AStationID  : integer;
                       APatchID    : integer);

  end;

implementation

uses
  UDataSetType,
  UConstants,
  UDBConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TSelectRawFlagsForm                                                        *}
{******************************************************************************}

procedure TSelectRawFlagsForm.CreateMemberObjects;
const OPNAME = 'TSelectRawFlagsForm.CreateMemberObjects';
begin
  try
    FParamField    := '';
    FStationID     := 0;
    FPatchID       := 0;

    Position := poScreenCenter;
    OnClose  := FormClose;
    OnShow   := FormShow;
    ClientHeight := 300;
    ClientWidth  := 300;

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
      Width      := 220;
      Height     := 220;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;
    FLblSelectRAWFlags := TLabel.Create(Self);
    with FLblSelectRAWFlags do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 10;
      Width    := 200;
      Height   := 40;
      WordWrap := TRUE;
    end;
    FClbSelectRAWFlags := TFieldCheckListBox.Create(Self, FAppModules);
    with FClbSelectRAWFlags do
    begin
      Parent       := FPnlClient;
      Left         := 10;
      Top          := 60;
      Width        := 200;
      Height       := 150;
      TabOrder     := 0;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSelectRawFlagsForm.Initialise: boolean;
const OPNAME = 'TSelectRawFlagsForm.Initialise';
begin
  Result := FALSE;
  try
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectRawFlagsForm.FormShow(Sender: TObject);
const OPNAME = 'TSelectRawFlagsForm.FormShow';
begin
  try
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSelectRawFlagsForm.SetData (AParamField : string;
                                       AStationID  : integer;
                                       APatchID    : integer);
const OPNAME = 'TSelectRawFlagsForm.SetData';
begin
  try
    FFieldProperty := nil;
    FParamField    := AParamField;
    FStationID     := AStationID;
    FPatchID       := APatchID;
    if (FParamField <> '') then
      FFieldProperty := FAppModules.FieldProperties.FieldProperty(FParamField);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSelectRawFlagsForm.OnResizeForm(Sender: TObject);
const OPNAME = 'TSelectRawFlagsForm.OnResizeForm';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSelectRawFlagsForm.DestroyMemberObjects;
const OPNAME = 'TSelectRawFlagsForm.DestroyMemberObjects';
begin
  try
    FFieldProperty := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSelectRawFlagsForm.LanguageHasChanged: boolean;
const OPNAME = 'TSelectRawFlagsForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Caption          := FAppModules.Language.GetString('Rainfall.SelectRAWFlags');
    FBtnOK.Hint      := FAppModules.Language.GetString('ButtonHint.CPOK');
    FBtnCancel.Hint  := FAppModules.Language.GetString('ButtonHint.CPCancel');

    FLblSelectRAWFlags.Caption  := FAppModules.Language.GetString('Rainfall.SelectRAWFlagsDescr');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectRawFlagsForm.StudyDataHasChanged (AContext: TChangeContext;
                                          AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSelectRawFlagsForm.StudyDataHasChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectRawFlagsForm.FormClose (Sender     : TObject;
                                          var Action : TCloseAction);
const OPNAME = 'TSelectRawFlagsForm.FormClose';
begin
  try
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectRawFlagsForm.PopulateControls;
const OPNAME = 'TSelectRawFlagsForm.PopulateControls';
var
  lIndex       : integer;
  LUpdateUser  : boolean;
  lRainfallObj : IRainfallModelData;
  lFlag        : string;
  lRawFlags    : TStringList;
  lAllFlags    : TStringList;
  lItemIdx     : integer;
begin
  try
    LUpdateUser := (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    LUpdateUser := LUpdateUser and (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));

    FBtnOK.Enabled     := LUpdateUser;
    FBtnCancel.Enabled := LUpdateUser;

    FClbSelectRAWFlags.Items.Clear;

    if (FFieldProperty <> nil) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lRawFlags := TStringList.Create;
      lAllFlags := TStringList.Create;
      try
        lAllFlags.CommaText := FFieldProperty.FieldAcceptedValues;
        for lIndex := 0 to lAllFlags.Count - 1 do
        begin
          lFlag := lAllFlags.Strings[lIndex];
          if (lFlag <> 'None') then
          begin
            lItemIdx := FClbSelectRAWFlags.Items.Add(lFlag);
            FClbSelectRAWFlags.ItemEnabled[lItemIdx] := (lFlag <> '[');
          end;
        end;
        lRawFlags.CommaText := lRainfallObj.RAWFlags;
        for lIndex := 0 to FClbSelectRAWFlags.Items.Count - 1 do
        begin
          lFlag := FClbSelectRAWFlags.Items[lIndex];
          if (lRawFlags.IndexOf(lFlag) >= 0) then
            FClbSelectRAWFlags.Checked[lIndex] := TRUE
          else
            FClbSelectRAWFlags.Checked[lIndex] := FALSE;
        end;
      finally
        FreeAndNil(lRawFlags);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectRawFlagsForm.OnOKClick(Sender: TObject);
const OPNAME = 'TSelectRawFlagsForm.OnOKClick';
var
  lRainfallObj  : IRainfallModelData;
  lRawFlags     : TStringList;
  lIndex        : integer;
begin
  try
    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    lRawFlags := TStringList.Create;
    try
      for lIndex := 0 to FClbSelectRAWFlags.Items.Count - 1 do
      begin
        if (FClbSelectRAWFlags.Checked[lIndex]) then
          lRawFlags.Add(FClbSelectRAWFlags.Items[lIndex]);
      end;
      lRainfallObj.RAWFlags := lRawFlags.CommaText;
    finally
      FreeAndNil(lRawFlags);
    end;
    ModalResult := mrOk;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectRawFlagsForm.OnCancelClick(Sender: TObject);
const OPNAME = 'TSelectRawFlagsForm.OnCancelClick';
begin
  try
    ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
