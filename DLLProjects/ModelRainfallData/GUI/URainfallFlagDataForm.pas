{******************************************************************************}
{*  UNIT      : Contains the class TRainfallFlagDataForm.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/06/05                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallFlagDataForm;

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
  RainfallCom_TLB,
  VCL.ImgList;

type
  TRainfallFlagDataForm = class(TAbstractForm)
  private
    FFlagMode          : Boolean;
    FParamField        : string;
    FFieldProperty     : TAbstractFieldProperty;
    FStationID         : integer;
    FPatchID           : integer;
    FStartYear         : integer;
    FEndYear           : integer;

    FScrollBox          : TScrollBox;
    FPnlButtons         : TAbstractPanel;
    FBtnOK              : TSpeedButton;
    FBtnCancel          : TSpeedButton;
    FPnlClient          : TAbstractPanel;
    FLblParamName       : TLabel;
    FLblParamNameVal    : TLabel;
    FLblEntityName      : TLabel;
    FLblEntityNameVal   : TLabel;
    FLblChangeList      : TLabel;
    FCbxChangeList      : TAbstractComboBox;
    FLblStartDate       : TLabel;
    FCbxStartYear       : TAbstractComboBox;
    FCbxStartMonth      : TAbstractComboBox;
    FLblEndDate         : TLabel;
    FCbxEndYear         : TAbstractComboBox;
    FCbxEndMonth        : TAbstractComboBox;
    FLblChange          : TLabel;
    FCbxChange          : TAbstractComboBox;
    FLblChangeDescr     : TLabel;
    FEdtChangeDescr     : TAbstractFieldEdit;
    procedure PopulateControls;
    function InputValid (var AChangeList  : IChangeList;
                         var AChange      : string;
                         var AChangeDescr : string;
                         var AStartYear   : integer;
                         var AStartMonth  : integer;
                         var AEndYear     : integer;
                         var AEndMonth    : integer): Boolean;
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
    procedure SetData (AFlagMode   : boolean;
                       AParamField : string;
                       AStationID  : integer;
                       APatchID    : integer);

  end;

implementation

uses
  System.UITypes,
  UDataSetType,
  UConstants,
  UDBConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TRainfallFlagDataForm                                                      *}
{******************************************************************************}

procedure TRainfallFlagDataForm.CreateMemberObjects;
const OPNAME = 'TRainfallFlagDataForm.CreateMemberObjects';
begin
  try
    FParamField    := '';
    FStationID     := 0;
    FPatchID       := 0;
    FStartYear     := 0;
    FEndYear       := 0;

    Position := poScreenCenter;
    OnClose  := FormClose;
    OnShow   := FormShow;
    ClientHeight := 350;
    ClientWidth  := 590;

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
      Height     := 250;
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
      Width    := 110;
      Height   := 13;
    end;
    FLblParamNameVal := TLabel.Create(Self);
    with FLblParamNameVal do
    begin
      Parent   := FPnlClient;
      AutoSize := TRUE;
      Left     := 120;
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
      Top      := 40;
      Width    := 110;
      Height   := 13;
    end;
    FLblEntityNameVal := TLabel.Create(Self);
    with FLblEntityNameVal do
    begin
      Parent   := FPnlClient;
      AutoSize := TRUE;
      Left     := 120;
      Top      := 40;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
    FLblChangeList := TLabel.Create(Self);
    with FLblChangeList do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 70;
      Width    := 110;
      Height   := 13;
    end;
    FCbxChangeList := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxChangeList do
    begin
      Parent       := FPnlClient;
      Left         := 120;
      Top          := 70;
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
      Top      := 100;
      Width    := 110;
      Height   := 13;
    end;
    FCbxChange := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxChange do
    begin
      Parent    := FPnlClient;
      Left      := 120;
      Top       := 100;
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
      Top      := 130;
      Width    := 110;
      Height   := 13;
    end;

    FEdtChangeDescr := TAbstractFieldEdit.Create(Self, FAppModules);
    with FEdtChangeDescr do
    begin
      Parent    := FPnlClient;
      Left      := 120;
      Top       := 130;
      Width     := 180;
      Height    := 21;
      TabOrder  := 1;
    end;

    FLblStartDate := TLabel.Create(Self);
    with FLblStartDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 160;
      Width    := 110;
      Height   := 13;
    end;
    FCbxStartYear := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxStartYear do
    begin
      Parent   := FPnlClient;
      Left     := 120;
      Top      := 160;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 2;
    end;
    FCbxStartMonth := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxStartMonth do
    begin
      Parent    := FPnlClient;
      Left      := 185;
      Top       := 160;
      Width     := 60;
      Height    := 21;
      Style     := csDropDownList;
      TabOrder  := 3;
    end;
    FLblEndDate := TLabel.Create(Self);
    with FLblEndDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 190;
      Width    := 110;
      Height   := 13;
    end;
    FCbxEndYear := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxEndYear do
    begin
      Parent   := FPnlClient;
      Left     := 120;
      Top      := 190;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 4;
    end;
    FCbxEndMonth := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxEndMonth do
    begin
      Parent   := FPnlClient;
      Left     := 185;
      Top      := 190;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 5;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallFlagDataForm.Initialise: boolean;
const OPNAME = 'TRainfallFlagDataForm.Initialise';
begin
  Result := FALSE;
  try
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagDataForm.FormShow(Sender: TObject);
const OPNAME = 'TRainfallFlagDataForm.FormShow';
begin
  try
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallFlagDataForm.SetData (AFlagMode   : boolean;
                                         AParamField : string;
                                         AStationID  : integer;
                                         APatchID    : integer);
const OPNAME = 'TRainfallFlagDataForm.SetData';
begin
  try
    FFlagMode      := AFlagMode;
    if (FFlagMode) then
      Caption := FAppModules.Language.GetString('Rainfall.FlagDataBlock')
    else
      Caption := FAppModules.Language.GetString('Rainfall.UnFlagDataBlock');
    FFieldProperty := nil;
    FParamField    := Trim(AParamField);
    FStationID     := AStationID;
    FPatchID       := APatchID;
    if (FParamField <> '') then
      FFieldProperty := FAppModules.FieldProperties.FieldProperty(FParamField);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallFlagDataForm.DestroyMemberObjects;
const OPNAME = 'TRainfallFlagDataForm.DestroyMemberObjects';
begin
  try
    FFieldProperty := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallFlagDataForm.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallFlagDataForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    if (FFlagMode) then
      Caption        := FAppModules.Language.GetString('Rainfall.FlagDataBlock')
    else
      Caption        := FAppModules.Language.GetString('Rainfall.UnFlagDataBlock');
    FBtnOK.Hint      := FAppModules.Language.GetString('ButtonHint.CPOK');
    FBtnCancel.Hint  := FAppModules.Language.GetString('ButtonHint.CPCancel');

    FLblParamName.Caption   := FAppModules.Language.GetString('ChangeLists.ParameterName') + ' :';
    FLblEntityName.Caption  := FAppModules.Language.GetString('ChangeLists.EntityDescr') + ' :';
    FLblChangeList.Caption  := FAppModules.Language.GetString('ChangeLists.ChangeList') + ' :';
    FLblStartDate.Caption   := FAppModules.Language.GetString('ChangeLists.StartYearMonth') + ' :';
    FLblEndDate.Caption     := FAppModules.Language.GetString('ChangeLists.EndYearMonth') + ' :';
    FLblChange.Caption      := FAppModules.Language.GetString('ChangeLists.Change') + ' :';
    FLblChangeDescr.Caption := FAppModules.Language.GetString('ChangeLists.Description') + ' :';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallFlagDataForm.StudyDataHasChanged (AContext: TChangeContext;
                                          AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TRainfallFlagDataForm.StudyDataHasChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagDataForm.FormClose (Sender     : TObject;
                                          var Action : TCloseAction);
const OPNAME = 'TRainfallFlagDataForm.FormClose';
begin
  try
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagDataForm.PopulateControls;
const OPNAME = 'TRainfallFlagDataForm.PopulateControls';
var
  lKeyValues   : string;
  lChangeList  : IChangeList;
  lIndex       : integer;
  LUpdateUser  : boolean;
  lRainfallObj : IRainfallModelData;
  lStation     : IStationData;
  lPatch       : IPatchData;
begin
  try
    LUpdateUser := (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    LUpdateUser := LUpdateUser and (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));

    FBtnOK.Enabled     := LUpdateUser;
    FBtnCancel.Enabled := LUpdateUser;

    FLblEntityNameVal.Caption := '';
    FLblParamNameVal.Caption  := '';

    FCbxChangeList.Items.Clear;
    for lIndex := 0 to FAppModules.Changes.ChangeLists.Count - 1 do
    begin
      lChangeList := FAppModules.Changes.ChangeListWithIndex(lIndex);
      FCbxChangeList.Items.AddObject(lChangeList.ChangeListName, TObject(lChangeList.ChangeListID));
    end;
    if (FCbxChangeList.Items.Count > 0) then
      FCbxChangeList.ItemIndex := 0
    else
      FCbxChangeList.ItemIndex := -1;

    if (FFieldProperty <> nil) then
    begin
      FCbxChange.Clear;
      FLblChange.Enabled := False;
      FCbxChange.Enabled := False;
      FCbxChange.Color   := clBtnFace;
      if FFlagMode then
      begin
        FLblChange.Enabled := True;
        FCbxChange.Enabled := True;
        FCbxChange.Color   := clWindow;
        if (FFieldProperty.FieldGroup in [fgValidation, fgValidationArray]) then
          FCbxChange.Items.CommaText := FFieldProperty.FieldAcceptedValues;
        lIndex := FCbxChange.Items.IndexOf('[');
        if (lIndex >= 0) then
          FCbxChange.Items.Delete(lIndex);
        if (FCbxChange.Items.Count > 0) then
          FCbxChange.ItemIndex := 0;
      end;

      lKeyValues := '';
      lStation   := nil;
      lPatch     := nil;
      if (FStationID <> 0) then
      begin
        lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
        lStation     := lRainfallObj.GetStationDataByID(FStationID);
        lKeyValues   := 'StationID=' + IntToStr(FStationID);
        if (FPatchID <> 0) then
        begin
          if (lStation <> nil) then
            lPatch := lStation.GetPatchWithID(FPatchID);
          lKeyValues := 'PatchID=' + IntToStr(FPatchID) + ',' + lKeyValues;
        end;
        if (lPatch <> nil) then
        begin
          FStartYear := lPatch.RainfallData.HydroStartYear;
          FEndYear   := lPatch.RainfallData.HydroEndYear + 1;
        end
        else
        if (lStation <> nil) then
        begin
          FStartYear := lStation.RainfallData.HydroStartYear;
          FEndYear   := lStation.RainfallData.HydroEndYear + 1;
        end
        else
        begin
          FStartYear := 0;
          FEndYear   := 0;
        end;
        FCbxStartYear.Clear;
        FCbxStartMonth.Clear;
        FCbxEndYear.Clear;
        FCbxEndMonth.Clear;
        for lIndex := 1 to 12 do
        begin
          FCbxStartMonth.Items.Add(FormatSettings.ShortMonthNames[lIndex]);
          FCbxEndMonth.Items.Add(FormatSettings.ShortMonthNames[lIndex]);
        end;
        if (FStartYear <> 0) then
        begin
          for lIndex := FStartYear to FEndYear do
          begin
            FCbxStartYear.Items.Add(IntToStr(lIndex));
            FCbxEndYear.Items.Add(IntToStr(lIndex));
          end;
        end;
      end;
      FLblEntityNameVal.Caption := FAppModules.Model.EntityDescription(FParamField, lKeyValues, '');
      FLblParamNameVal.Caption  := FParamField;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallFlagDataForm.InputValid (var AChangeList  : IChangeList;
                                           var AChange      : string;
                                           var AChangeDescr : string;
                                           var AStartYear   : integer;
                                           var AStartMonth  : integer;
                                           var AEndYear     : integer;
                                           var AEndMonth    : integer): Boolean;
const OPNAME = 'TRainfallFlagDataForm.InputValid';
var
  lIndex       : integer;
  lMessage     : string;
begin
  Result := FALSE;
  try
    Result   := TRUE;

    AChangeList  := nil;
    AChange      := '';
    AChangeDescr := '';
    AStartYear   := 0;
    AStartMonth  := 0;
    AEndYear     := 0;
    AEndMonth    := 0;

    lIndex := FCbxChangeList.ItemIndex;
    if (lIndex < 0) then
    begin
      Result := FALSE;
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectChangeList'));
    end
    else
    begin
      AChangeList := FAppModules.Changes.ChangeListWithIndex(lIndex);
    end;
    if (Result) then
    begin
      AChange := FCbxChange.Text;
      AChangeDescr := FEdtChangeDescr.Text;
      if (FFlagMode)and (AChange = 'None') then
      begin
        lMessage := FAppModules.Language.GetString('Rainfall.ConfirmSelectNoFlag');
        if (MessageDlg(lMessage, mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
        begin
          Result := FALSE;
          ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectChangeFlag'));
        end;
      end
      else
      if (FFlagMode) and(AChange = '') then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectChangeFlag'));
      end;
    end;
    if (Result) then
    begin
      if (FCbxStartYear.ItemIndex >= 0) then
        AStartYear := StrToInt(FCbxStartYear.Text)
      else
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectStartYear'));
      end;
    end;
    if (Result) then
    begin
      if (FCbxStartMonth.ItemIndex >= 0) then
        AStartMonth := FCbxStartMonth.ItemIndex + 1
      else
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectStartMonth'));
      end;
    end;
    if (Result) then
    begin
      if ((FCbxStartYear.ItemIndex = 0) AND (FCbxStartMonth.ItemIndex < 9)) OR
         ((FCbxStartYear.ItemIndex = FCbxStartYear.Items.Count-1) AND (FCbxStartMonth.ItemIndex > 9)) then
      begin
        Result := FALSE;
        lMessage := FAppModules.Language.GetString('ChangeLists.StartDateOutOfRange');
        lMessage := Format(lMessage, [IntToStr(FStartYear) + '/10',
                                      IntToStr(FEndYear) + '/09']);
        ShowMessage(lMessage);
      end;
    end;
    if (Result) then
    begin
      if (FCbxEndYear.ItemIndex >= 0) then
        AEndYear := StrToInt(FCbxEndYear.Text)
      else
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectEndYear'));
      end;
    end;
    if (Result) then
    begin
      if (FCbxEndMonth.ItemIndex >= 0) then
        AEndMonth := FCbxEndMonth.ItemIndex + 1
      else
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectEndMonth'));
      end;
    end;
    if (Result) then
    begin
      if ((FCbxEndYear.ItemIndex = 0) AND (FCbxEndMonth.ItemIndex < 9)) OR
         ((FCbxEndYear.ItemIndex = FCbxEndYear.Items.Count-1) AND (FCbxEndMonth.ItemIndex > 9)) then
      begin
        Result := FALSE;
        lMessage := FAppModules.Language.GetString('ChangeLists.EndDateOutOfRange');
        lMessage := Format(lMessage, [IntToStr(FStartYear) + '/Oct',
                                      IntToStr(FEndYear) + '/Sep']);
        ShowMessage(lMessage);
      end;
    end;
    if (Result) then
    begin
      if (AEndYear < AStartYear) OR
         ((AEndYear = AStartYear) AND (AEndMonth < AStartMonth)) then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.EndDateBeforeStartDate'));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagDataForm.OnCancelClick(Sender: TObject);
const OPNAME = 'TRainfallFlagDataForm.OnCancelClick';
begin
  try
    ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallFlagDataForm.OnOKClick(Sender: TObject);
const OPNAME = 'TRainfallFlagDataForm.OnOKClick';
var
  lChange       : string;
  lChangeDescr  : string;
  lChangeList   : IChangeList;
  lStartYear    : integer;
  lStartMonth   : integer;
  lEndYear      : integer;
  lEndMonth     : integer;
  lYear         : integer;
  lMonth        : integer;
  lMinMonth     : integer;
  lMaxMonth     : integer;
  lKeyValues    : string;
  lFieldIndex   : string;
  lRainfallObj  : IRainfallModelData;
  lStation      : IStationData;
  lPatch        : IPatchData;
  lMessageList  : TStringList;
  lMessage      : string;
  lParamChange  : IParameterChange;
begin
  try
    lStation := nil;
    lPatch   := nil;
    lMessageList := TStringList.Create;
    try
      if (InputValid(lChangeList, lChange, lChangeDescr, lStartYear, lStartMonth, lEndYear, lEndMonth)) then
      begin
        if (lChange = 'None') then
          lChange := '';
        if (lChangeList <> nil) AND (FStationID <> 0) then
        begin
          lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
          lStation     := lRainfallObj.GetStationDataByID(FStationID);
          if (lStation <> nil) then
          begin
            if (FPatchID <> 0) then
              lPatch := lStation.GetPatchWithID(FPatchID);
            lMessageList.Clear;
            for lYear := lStartYear to lEndYear do
            begin
              lFieldIndex := IntToStr(lYear);
              if (lPatch <> nil) then
                lKeyValues := lPatch.GetKeyValues(FParamField, lFieldIndex)
              else
                lKeyValues := lStation.GetKeyValues(FParamField, lFieldIndex);
              if (lYear = lStartYear) then
                lMinMonth := lStartMonth
              else
                lMinMonth := 1;
              if (lYear = lEndYear) then
                lMaxMonth := lEndMonth
              else
                lMaxMonth := 12;

              for lMonth := lMinMonth to lMaxMonth do
              begin
                lParamChange := lChangeList.FindParamChange(FParamField, lKeyValues, IntToStr(lMonth));
                if (FFlagMode) then
                begin
                  if (lParamChange <> nil) then
                  begin
                    lMessage := FAppModules.Language.GetString('ChangeLists.AlreadyContainsFlag');
                    lMessage := Format(lMessage, [lChangeList.ChangeListName,
                                 FAppModules.Model.EntityDescription(FParamField, lKeyValues, IntToStr(lMonth))]);
                    ShowMessage(lMessage);
                  end
                  else
                    lChangeList.CreateNewParamChange(FParamField, lKeyValues, IntToStr(lMonth), 'Y', lChange,lChangeDescr,False);
                end
                else
                begin
                  if (lParamChange <> nil) then
                  begin
                    lParamChange := nil;
                    lChangeList.DeleteParamChange(FParamField, lKeyValues, IntToStr(lMonth));
                  end
                  else
                  begin
                      lMessage := FAppModules.Language.GetString('ChangeLists.DoesNotContainFlag');
                      LMessageList.Add(Format(lMessage, [lChangeList.ChangeListName, lChange,
                                 FAppModules.Model.EntityDescription(FParamField, lKeyValues, IntToStr(lMonth))]));
                  end;
                end;
              end;
              if (lYear = lEndYear) and (LMessageList.Count>0) then
                ShowMessage(LMessageList.Text);
            end;
          end;
        end;
        ModalResult := mrOk;
      end;
    finally
      FreeAndNil(lMessageList);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
