{******************************************************************************}
{*  UNIT      : Contains the class TSplitRainfallRecordForm.                  *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/07/21                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit USplitRainfallRecordForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids,Buttons,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChildToolbar,
  ImgList;

type
  TSplitRainfallRecordForm = class(TAbstractForm)
  private
    FStationID         : integer;
    FStartYear         : integer;
    FEndYear           : integer;

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

    procedure PopulateControls;
    function InputValid (var AStartYear  : integer;
                         var AEndYear    : integer): Boolean;
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
    procedure SetData (AStationID  : integer);

  end;

implementation

uses
  UDataSetType,
  UConstants,
  UDBConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TSplitRainfallRecordForm                                                   *}
{******************************************************************************}

procedure TSplitRainfallRecordForm.CreateMemberObjects;
const OPNAME = 'TSplitRainfallRecordForm.CreateMemberObjects';
begin
  try
    FStationID     := 0;
    FStartYear     := 0;
    FEndYear       := 0;

    Position := poScreenCenter;
    OnClose  := FormClose;
    OnShow   := FormShow;
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
//    Self.OnResize := OnResizeForm;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSplitRainfallRecordForm.Initialise: boolean;
const OPNAME = 'TSplitRainfallRecordForm.Initialise';
begin
  Result := FALSE;
  try
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSplitRainfallRecordForm.FormShow(Sender: TObject);
const OPNAME = 'TSplitRainfallRecordForm.FormShow';
begin
  try
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSplitRainfallRecordForm.SetData (AStationID  : integer);
const OPNAME = 'TSplitRainfallRecordForm.SetData';
begin
  try
    FStationID := AStationID;
    Caption := FAppModules.Language.GetString('Rainfall.SplitRainfallRecord')
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSplitRainfallRecordForm.OnResizeForm(Sender: TObject);
const OPNAME = 'TSplitRainfallRecordForm.OnResizeForm';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TSplitRainfallRecordForm.DestroyMemberObjects;
const OPNAME = 'TSplitRainfallRecordForm.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TSplitRainfallRecordForm.LanguageHasChanged: boolean;
const OPNAME = 'TSplitRainfallRecordForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Caption := FAppModules.Language.GetString('Rainfall.SplitRainfallRecord');
    FBtnOK.Hint      := FAppModules.Language.GetString('ButtonHint.CPOK');
    FBtnCancel.Hint  := FAppModules.Language.GetString('ButtonHint.CPCancel');

    FLblStationName.Caption := FAppModules.Language.GetString('Rainfall.StationNumber') + ' :';
    FLblStartDate.Caption   := FAppModules.Language.GetString('Rainfall.HydroStartYear') + ' :';
    FLblEndDate.Caption     := FAppModules.Language.GetString('Rainfall.HydroEndYear') + ' :';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSplitRainfallRecordForm.StudyDataHasChanged (AContext: TChangeContext;
                                          AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSplitRainfallRecordForm.StudyDataHasChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSplitRainfallRecordForm.FormClose (Sender     : TObject;
                                              var Action : TCloseAction);
const OPNAME = 'TSplitRainfallRecordForm.FormClose';
begin
  try
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSplitRainfallRecordForm.PopulateControls;
const OPNAME = 'TSplitRainfallRecordForm.PopulateControls';
var
  LUpdateUser  : boolean;
  lRainfallObj : IRainfallModelData;
  lStation     : IStationData;
  lIndex       : integer;
begin
  try
    LUpdateUser := (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    LUpdateUser := LUpdateUser and (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));

    FBtnOK.Enabled     := LUpdateUser;
    FBtnCancel.Enabled := LUpdateUser;

    FLblStationNameVal.Caption := '';

    lStation   := nil;
    if (FStationID <> 0) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lStation     := lRainfallObj.GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        FLblStationNameVal.Caption := lStation.RainfallData.StationNumber;
        if (lStation.IsInWR90) then
          FLblStationNameVal.Caption := FLblStationNameVal.Caption + '*';
        FStartYear   := lStation.RainfallData.HydroStartYear;
        FEndYear     := lStation.RainfallData.HydroEndYear;
        FCbxStartYear.Clear;
        FCbxEndYear.Clear;
        if (FStartYear <> 0) then
        begin
          for lIndex := FStartYear to FEndYear do
          begin
            FCbxStartYear.Items.Add(IntToStr(lIndex));
            FCbxEndYear.Items.Add(IntToStr(lIndex));
          end;
          FCbxStartYear.ItemIndex := 0;
          FCbxEndYear.ItemIndex   := 0;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSplitRainfallRecordForm.InputValid (var AStartYear  : integer;
                                              var AEndYear    : integer): Boolean;
const OPNAME = 'TSplitRainfallRecordForm.InputValid';
var
  lMessage     : string;
  lRainfallObj : IRainfallModelData;
  lStation     : IStationData;
begin
  Result := FALSE;
  try
    Result      := TRUE;
    AStartYear  := 0;

    if (FCbxStartYear.ItemIndex >= 0) then
      AStartYear := StrToInt(FCbxStartYear.Text)
    else
    begin
      Result := FALSE;
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectStartYear'));
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
      if (AEndYear < AStartYear) then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('Rainfall.EndYearBeforeStartYear'));
      end;
    end;
    if (Result) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lStation     := lRainfallObj.GetStationDataByID(FStationID);
      if (lStation <> nil) AND (lStation.GetSplitForYears(AStartYear, AEndYear) <> nil) then
      begin
        Result := FALSE;
        lMessage := FAppModules.Language.GetString('Rainfall.StationAlreadyContainsSplit');
        lMessage := Format(lMessage, [lStation.RainfallData.StationNumber, IntToStr(AStartYear), IntToStr(AEndYear)]);
        ShowMessage(lMessage);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSplitRainfallRecordForm.OnOKClick(Sender: TObject);
const OPNAME = 'TSplitRainfallRecordForm.OnOKClick';
var
  lStartYear    : integer;
  lEndYear      : integer;
  lRainfallObj  : IRainfallModelData;
  lStation      : IStationData;
begin
  try
    lStation := nil;
    if (FStationID <> 0) then
    begin
      lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
      lStation     := lRainfallObj.GetStationDataByID(FStationID);
      if (lStation <> nil) then
      begin
        if (InputValid(lStartYear, lEndYear)) then
        begin
          lRainfallObj.CreateASplit(lStation.RainfallData.StationID, lStartYear, lEndYear);
        end;
      end;
      ModalResult := mrOk;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSplitRainfallRecordForm.OnCancelClick(Sender: TObject);
const OPNAME = 'TSplitRainfallRecordForm.OnCancelClick';
begin
  try
    ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
