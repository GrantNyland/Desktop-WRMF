{******************************************************************************}
{*  UNIT      : Contains the class TMetaDataForm.                             *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/03/09                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UMetaDataForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Grids,Vcl.Buttons,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChildToolbar,
  UMetaData,
  VoaimsCom_TLB,
  Vcl.ImgList;

type
  TMetaDataForm = class(TAbstractForm)
  private
    FParamField          : string;
    FKeyValues           : string;
    FFieldIndex          : string;
    FFieldProperty       : TAbstractFieldProperty;
    FActiveControl       : TWinControl;

    FScrollBox           : TScrollBox;
    FPnlButtons          : TAbstractPanel;
    FBtnNew              : TSpeedButton;
    FBtnDelete           : TSpeedButton;
    FBtnCLose            : TSpeedButton;
    FPnlClient           : TAbstractPanel;
    FLblParamName        : TLabel;
    FLblParamNameVal     : TLabel;
    FLblEntityName       : TLabel;
    FLblEntityNameVal    : TLabel;
    FPnlMetaData         : TAbstractPanel;
    FLblCreatedBy        : TLabel;
    FEdtCreatedBy        : TFieldEdit;
    FLblDateCreated      : TLabel;
    FLblDateCreatedValue : TLabel;
    FLblComment          : TLabel;
    FMmoComment          : TFieldRichEdit{TAbstractRichEdit};

    procedure PopulateControls;
    procedure ResetControls;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FormClose (Sender     : TObject;
                         var Action : TCloseAction);
    procedure FormShow (Sender: TObject);
    procedure OnResizeForm (Sender : TObject);
    procedure OnNewClick (Sender : TObject);
    procedure OnDeleteClick(Sender: TObject);
    procedure OnCloseClick (Sender : TObject);
    procedure OnEdtCreatedByExit(Sender: TObject);
    procedure OnMmoCommentExit(Sender: TObject);
    procedure OnControlEnter(Sender: TObject);
    procedure AssignHelpContext; override;
  public
    function StudyDataHasChanged(AContext: TChangeContext;
                                 AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetData (AParamField : string;
                       AKeyValues  : string;
                       AFieldIndex : string);

  end;

implementation

uses
  UDataSetType,
  UConstants,
  UHelpContexts,
  UDBConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TMetaDataForm                                                              *}
{******************************************************************************}

procedure TMetaDataForm.CreateMemberObjects;
const OPNAME = 'TMetaDataForm.CreateMemberObjects';
begin
  try
    FParamField    := '';
    FKeyValues     := '';
    FFieldIndex    := '';

    Position := poScreenCenter;
    OnClose  := FormClose;
    OnShow   := FormShow;

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
    FBtnNew := TSpeedButton.Create(FPnlButtons);
    FBtnNew.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('MetaDataCreateNew'));
    with FBtnNew do
    begin
      Parent   := FPnlButtons;
      Left     := 0;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 0;
      ShowHint := TRUE;
      OnClick  := OnNewClick;
    end;
    FBtnDelete := TSpeedButton.Create(FPnlButtons);
    FBtnDelete.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('MetaDataDelete'));
    with FBtnDelete do
    begin
      Parent   := FPnlButtons;
      Left     := 30;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 2;
      ShowHint := TRUE;
      OnClick  := OnDeleteClick;
    end;
    FBtnClose := TSpeedButton.Create(FPnlButtons);
    FBtnClose.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPClose'));
    with FBtnClose do
    begin
      Parent      := FPnlButtons;
      Left        := 60;
      Top         := 0;
      Width       := 30;
      Height      := 30;
      TabOrder    := 5;
      ModalResult := 1;
      ShowHint    := TRUE;
      OnClick     := OnCloseClick;
    end;
    FPnlClient := TAbstractPanel.Create(Self, FAppModules);
    with FPnlClient do
    begin
      Parent     := FScrollBox;
      Left       := 0;
      Top        := 30;
      Width      := 420;
      Height     := 230;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;
    FLblParamName := TLabel.Create(Self);
    with FLblParamName do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 10;
      Width    := 100;
      Height   := 13;
    end;
    FLblParamNameVal := TLabel.Create(Self);
    with FLblParamNameVal do
    begin
      Parent   := FPnlClient;
      AutoSize := TRUE;
      Left     := 100;
      Top      := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
    FLblEntityName := TLabel.Create(Self);
    with FLblEntityName do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 30;
      Width    := 100;
      Height   := 13;
    end;
    FLblEntityNameVal := TLabel.Create(Self);
    with FLblEntityNameVal do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 100;
      Top      := 30;
      Width    := 310;
      Height   := 26;
      WordWrap := TRUE;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
    FPnlMetaData := TAbstractPanel.Create(Self, FAppModules);
    with FPnlMetaData do
    begin
      Parent     := FPnlClient;
      Left       := 0;
      Top        := 60;
      Width      := 420;
      Height     := 170;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;
    FlblDateCreated := TLabel.Create(Self);
    with FlblDateCreated do
    begin
      Parent   := FPnlMetaData;
      Left     := 10;
      Top      := 0;
      Width    := 68;
    end;
    FlblDateCreatedValue := TLabel.Create(Self);
    with FlblDateCreatedValue do
    begin
      Parent   := FPnlMetaData;
      Left     := 100;
      Top      := 0;
      Width    := 58;
    end;
    FLblCreatedBy := TLabel.Create(Self);
    with FLblCreatedBy do
    begin
      Parent   := FPnlMetaData;;
      Left     := 10;
      Top      := 20;
      Width    := 57;
    end;
    FEdtCreatedBy := TFieldEdit.Create(Self, FAppModules);
    with FEdtCreatedBy do
    begin
      Parent   := FPnlMetaData;
      Left     := 100;
      Top      := 20;
      Width    := 310;
      TabOrder := 1;
    end;
    FLblComment := TLabel.Create(Self);
    with FLblComment do
    begin
      Parent   := FPnlMetaData;
      Left     := 10;
      Top      := 40;
      Width    := 59;
    end;
    FMmoComment := TFieldRichEdit.Create(Self, FAppModules);
    with FMmoComment do
    begin
      Parent     := FPnlMetaData;
      Left       := 10;
      Top        := 60;
      Width      := 400;
      Height     := 100;
      ScrollBars := ssVertical;
      TabOrder := 2;
    end;
    FEdtCreatedBy.FieldProperty := FAppModules.FieldProperties.FieldProperty('MetaDataCreatedBy');
    FMmoComment.FieldProperty   := FAppModules.FieldProperties.FieldProperty('MetaDataComment');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataForm.Initialise: boolean;
const OPNAME = 'TMetaDataForm.Initialise';
begin
  Result := inherited Initialise;
  try
    FEdtCreatedBy.OnExit  := OnEdtCreatedByExit;
    FEdtCreatedBy.OnEnter := OnControlEnter;
    FMmoComment.OnExit    := OnMmoCommentExit;
    FMmoComment.OnEnter   := OnControlEnter;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.FormShow(Sender: TObject);
const OPNAME = 'TMetaDataForm.FormShow';
begin
  try
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaDataForm.SetData (AParamField : string;
                                 AKeyValues  : string;
                                 AFieldIndex : string);
const OPNAME = 'TMetaDataForm.SetData';
begin
  try
    FActiveControl := nil;
    FFieldProperty := nil;
    FParamField    := Trim(AParamField);
    FFieldIndex    := AFieldIndex;
    FKeyValues     := AKeyValues;
    if (FParamField <> '') then
      FFieldProperty := FAppModules.FieldProperties.FieldProperty(FParamField);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaDataForm.OnResizeForm(Sender: TObject);
const OPNAME = 'TMetaDataForm.OnResizeForm';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TMetaDataForm.DestroyMemberObjects;
const OPNAME = 'TMetaDataForm.DestroyMemberObjects';
begin
  try
    FFieldProperty := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TMetaDataForm.LanguageHasChanged: boolean;
const OPNAME = 'TMetaDataForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Caption          := FAppModules.Language.GetString('MetaData.MetaData');
    FBtnNew.Hint     := FAppModules.Language.GetString('ButtonHint.MetaDataCreateNew');
    FBtnDelete.Hint  := FAppModules.Language.GetString('ButtonHint.MetaDataDelete');
    FBtnClose.Hint   := FAppModules.Language.GetString('ButtonHint.CPClose');

    FLblParamName.Caption   := FAppModules.Language.GetString('ChangeLists.ParameterName') + ' :';
    FLblEntityName.Caption  := FAppModules.Language.GetString('ChangeLists.EntityDescr') + ' :';
    FlblDateCreated.Caption := FAppModules.Language.GetString('ChangeLists.DateCreated') + ' :';
    FLblCreatedBy.Caption   := FAppModules.Language.GetString('ChangeLists.CreatedBy') + ' :';
    FLblComment.Caption     := FAppModules.Language.GetString('MetaData.Comment') + ' :';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMetaDataForm.StudyDataHasChanged (AContext: TChangeContext;
                                          AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TMetaDataForm.StudyDataHasChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.FormClose (Sender     : TObject;
                                   var Action : TCloseAction);
const OPNAME = 'TMetaDataForm.FormClose';
begin
  try
    if (FActiveControl = FEdtCreatedBy) then
      OnEdtCreatedByExit(FEdtCreatedBy)
    else
    if (FActiveControl = FMmoComment) then
      OnMmoCommentExit(FMmoComment);
    ModalResult := mrOk;
    Action      := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.PopulateControls;
const OPNAME = 'TMetaDataForm.PopulateControls';
var
  lMetaData : IMetaData;
begin
  try
    if (FFieldProperty <> nil) then
    begin
      if (FFieldProperty.DataClassName = 'TStudyFields') then
        FLblEntityNameVal.Caption := FAppModules.Language.GetString('WaterUse.Scenario')
      else
      if (FParamField = 'ChangeListItem') then
        FLblEntityNameVal.Caption := FAppModules.Changes.EntityDescription(FParamField, FKeyValues, FFieldIndex)
      else
        FLblEntityNameVal.Caption := FAppModules.Model.EntityDescription(FParamField, FKeyValues, FFieldIndex);
      if (FParamField = 'ReservoirPenalty') then
        FLblParamNameVal.Caption := FParamField + '[' + FAppModules.Changes.GetKeyValue('Identifier', FKeyValues) + ']'
      else
      if (FFieldIndex <> '') then
        FLblParamNameVal.Caption := FParamField + '[' + FFieldIndex + ']'
      else
        FLblParamNameVal.Caption := FParamField;
      lMetaData := FAppModules.MetaData.FindMetaData(FParamField, FKeyValues, FFieldIndex);
      FMmoComment.Lines.Clear;
      if (lMetaData <> nil) then
      begin
        FPnlMetaData.Visible := TRUE;
        FLblDateCreatedValue.Caption := DateToStr(lMetaData.DateCreated);
        FEdtCreatedBy.Text           := lMetaData.CreatedBy;
        FMmoComment.Lines.Add(lMetaData.Comment);
      end
      else
      begin
        FPnlMetaData.Visible := FALSE;
      end;
      ResetControls;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.ResetControls;
const OPNAME = 'TMetaDataForm.ResetControls';
var
  lMetaData : IMetaData;
begin
  try
    if (FFieldProperty <> nil) then
      lMetaData := FAppModules.MetaData.FindMetaData(FParamField, FKeyValues, FFieldIndex);

    FBtnNew.Enabled    := (lMetaData = nil);
    FBtnDelete.Enabled := (lMetaData <> nil);
    FBtnClose.Enabled  := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.OnNewClick(Sender: TObject);
const OPNAME = 'TMetaDataForm.OnNewClick';
begin
  try
    FAppModules.MetaData.CreateNewMetaData(FParamField, FKeyValues, FFieldIndex);
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.OnDeleteClick(Sender: TObject);
const OPNAME = 'TMetaDataForm.OnDeleteClick';
begin
  try
    FAppModules.MetaData.DeleteMetaData(FParamField, FKeyValues, FFieldIndex);
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.OnCloseClick(Sender: TObject);
const OPNAME = 'TMetaDataForm.OnCloseClick';
begin
  try
    Close;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.OnControlEnter(Sender: TObject);
const OPNAME = 'TMetaDataForm.OnControlEnter';
begin
  try
    if (Sender = FEdtCreatedBy) then
      FActiveControl := TFieldEdit(Sender)
    else
    if (Sender = FMmoComment) then
      FActiveControl := TAbstractRichEdit(Sender);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.OnEdtCreatedByExit(Sender: TObject);
const OPNAME = 'TMetaDataForm.OnEdtCreatedByExit';
var
  lCreatedBy : string;
  lMetaData  : IMetaData;
begin
  try
    lMetaData := FAppModules.MetaData.FindMetaData(FParamField, FKeyValues, FFieldIndex);
    if (lMetaData <> nil) then
    begin
      lCreatedBy := Trim(FEdtCreatedBy.Text);
      if (lCreatedBy = '') then
      begin
        ShowMessage(FAppModules.Language.GetString('MetaData.PleaseEnterPerson'));
        FEdtCreatedBy.SetFocus;
      end
      else
      begin
        if (lMetaData.CreatedBy <> lCreatedBy) then
          lMetaData.CreatedBy := lCreatedBy;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.OnMmoCommentExit(Sender: TObject);
const OPNAME = 'TMetaDataForm.OnMmoCommentExit';
var
  lComment     : string;
  lMetaData    : IMetaData;
begin
  try
    lMetaData := FAppModules.MetaData.FindMetaData(FParamField, FKeyValues, FFieldIndex);
    if (lMetaData <> nil) then
    begin
      lComment := Trim(FMmoComment.Text);
      if (lComment = '') then
      begin
        ShowMessage(FAppModules.Language.GetString('MetaData.PleaseEnterComment'));
        FMmoComment.SetFocus;
      end
      else
      begin
        if (lMetaData.Comment <> lComment) then
          lMetaData.Comment := lComment;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMetaDataForm.AssignHelpContext;
const OPNAME = 'TMetaDataForm.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,          HC_RunDescription);
    SetControlHelpContext(FEdtCreatedBy, HC_RunDescription);
    SetControlHelpContext(FMmoComment,   HC_RunDescription);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
