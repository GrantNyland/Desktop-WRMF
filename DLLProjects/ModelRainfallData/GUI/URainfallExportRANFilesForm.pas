{******************************************************************************}
{*  UNIT      : Contains the class TRainfallExportRANFilesForm                *}
{*  AUTHOR    : Samora Nxopho                                                 *}
{*  DATE      : 2005/07/26                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}
unit URainfallExportRANFilesForm;

interface
{$WARN UNIT_PLATFORM OFF}

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls, VCL.ComCtrls, VCL.Grids, VCL.Buttons, VCL.FileCtrl,

  UUtilities,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChildToolbar,
  UCatchmentZone,
  UDataObjectRainfall,
  RainfallCom_TLB,
  VCL.ImgList;

type
  TRainfallExportRANFilesForm = class(TAbstractForm)
  private
    FStartYear          : integer;
    FEndYear            : integer;
    FDirectory          : string;
    FStationID          : integer;
    FPatchID            : integer;
    FCatchmentZone      : TCatchmentZone;

    FScrollBox          : TScrollBox;
    FPnlButtons         : TAbstractPanel;
    FBtnOK              : TSpeedButton;
    FBtnCancel          : TSpeedButton;
    FPnlClient          : TAbstractPanel;
    FLblStartDate       : TLabel;
    FCbxStartYear       : TAbstractComboBox;
    FLblEndDate         : TLabel;
    FCbxEndYear         : TAbstractComboBox;
    FLblDirectory       : TLabel;
    FLblDirectoryValue  : TLabel;
    FCbxDrive           : TDriveComboBox;
    FLbxDirectory       : TDirectoryListBox;
    FRANChkBox          : TCheckBox;
    FOutputChkBox       : TCheckBox;
    FbtnCreateDir       : TSpeedButton;
    procedure PopulateControls;
    function InputValid : Boolean;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FormClose (Sender     : TObject;
                         var Action : TCloseAction);
    procedure FormShow (Sender: TObject);
    procedure OnResizeForm (Sender : TObject);
    procedure OnCancelClick(Sender: TObject);
    procedure OnOKClick(Sender: TObject);
    procedure OnDirectoryChange (Sender : TObject);
    procedure OnbtnCreateDirClick(Sender : TObject);
  public
    function StudyDataHasChanged(AContext: TChangeContext;
                                 AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetData (ACatchmentZone: TCatchmentZone);
    property StartYear  : integer read FStartYear;
    property EndYear    : integer read FEndYear;
    property Directory  : string  read FDirectory;
    property StationID  : integer read FStationID;
    property PatchID    : integer read FPatchID;
    property CatchmentZone: TCatchmentZone read FCatchmentZone;

  end;

implementation

uses
  UDataSetType,
  UConstants,
  UDBConstants,
  UErrorHandlingOperations;

{******************************************************************************}
{* TRainfallCreateFilesForm                                                   *}
{******************************************************************************}

procedure TRainfallExportRANFilesForm.CreateMemberObjects;
const OPNAME = 'TRainfallExportRANFilesForm.CreateMemberObjects';
begin
  try
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
      Width      := 400;
      Height     := 230;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;

    FRANChkBox := TCheckBox.Create(Self);
    with FRANChkBox do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 220;
      Top      := 10;
      Width    := 80;
      Caption  := FAppModules.Language.GetString('Rainfall.RANFile');
    end;
    FOutputChkBox := TCheckBox.Create(Self);
    with FOutputChkBox do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 220;
      Top      := 30;
      Width    := 80;
      Caption  := FAppModules.Language.GetString('Rainfall.OutPutFile');
    end;
    FLblStartDate := TLabel.Create(Self);
    with FLblStartDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 10;
      Width    := 110;
      Height   := 13;
    end;
    FCbxStartYear := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxStartYear do
    begin
      Parent   := FPnlClient;
      Left     := 130;
      Top      := 10;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 0;
      Enabled  := False;
    end;
    FLblEndDate := TLabel.Create(Self);
    with FLblEndDate do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 40;
      Width    := 110;
      Height   := 13;
    end;
    FCbxEndYear := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxEndYear do
    begin
      Parent   := FPnlClient;
      Left     := 130;
      Top      := 40;
      Width    := 60;
      Height   := 21;
      Style    := csDropDownList;
      TabOrder := 1;
      Enabled  := False;
    end;
    FLblDirectory := TLabel.Create(Self);
    with FLblDirectory do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 70;
      Width    := 110;
      Height   := 13;
    end;
    FLblDirectoryValue := TLabel.Create(Self);
    with FLblDirectoryValue do
    begin
      Parent   := FPnlClient;
      AutoSize := TRUE;
      Left     := 130;
      Top      := 70;
      Height   := 13;
    end;
    FLbxDirectory := TDirectoryListBox.Create(Self);
    with FLbxDirectory do
    begin
      Parent   := FPnlClient;
      Left     := 130;
      Top      := 115;
      Width    := 145;
      Height   := 100;
      OnChange := OnDirectoryChange;
      TabOrder := 3;
    end;
    FCbxDrive := TDriveComboBox.Create(Self);
    with FCbxDrive do
    begin
      Parent   := FPnlClient;
      Left     := 130;
      Top      := 90;
      Width    := 145;
      Height   := 19;
      DirList  := FLbxDirectory;
      TabOrder := 2;
    end;
    FbtnCreateDir := TSpeedButton.Create(FPnlButtons);
    FbtnCreateDir.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CreateDirectory'));
    with FbtnCreateDir do
    begin
      Parent   := FPnlClient;
      Left     := 275;
      Top      := 90;
      Width    := 20;
      Height   := 20;
      TabOrder := 3;
      ShowHint := True;
      OnClick  := OnbtnCreateDirClick;
    end;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallExportRANFilesForm.OnbtnCreateDirClick(Sender : TObject);
const OPNAME = 'TRainfallExportRANFilesForm.OnbtnCreateDirClick';
var
  LFolder : string;
begin
  try
    LFolder := InputBox('Enter New Directory','New Directory','');
    if (LFolder <> '') then
      if not SysUtils.DirectoryExists(FDirectory+'\'+LFolder) then
      begin
        SysUtils.ForceDirectories(FDirectory+'\'+LFolder);
        FDirectory := FDirectory+'\'+LFolder;
        FLbxDirectory.Directory := FDirectory;;
      end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallExportRANFilesForm.Initialise: boolean;
const OPNAME = 'TRainfallExportRANFilesForm.Initialise';
begin
  Result := FALSE;
  try
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallExportRANFilesForm.FormShow(Sender: TObject);
const OPNAME = 'TRainfallExportRANFilesForm.FormShow';
begin
  try
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallExportRANFilesForm.SetData (ACatchmentZone: TCatchmentZone);
const OPNAME = 'TRainfallExportRANFilesForm.SetData';
begin
  try
    FCatchmentZone  := ACatchmentZone;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallExportRANFilesForm.OnResizeForm(Sender: TObject);
const OPNAME = 'TRainfallExportRANFilesForm.OnResizeForm';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TRainfallExportRANFilesForm.DestroyMemberObjects;
const OPNAME = 'TRainfallExportRANFilesForm.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TRainfallExportRANFilesForm.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallExportRANFilesForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Caption          := FAppModules.Language.GetString('Rainfall.ExportRANFiles');
    FBtnOK.Hint      := FAppModules.Language.GetString('ButtonHint.CPOK');
    FBtnCancel.Hint  := FAppModules.Language.GetString('ButtonHint.CPCancel');

    FLblStartDate.Caption   := FAppModules.Language.GetString('Rainfall.HydroStartYear') + ' :';
    FLblEndDate.Caption     := FAppModules.Language.GetString('Rainfall.HydroEndYear') + ' :';
    FLblDirectory.Caption   := FAppModules.Language.GetString('TField.OutputPath') + ' :';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallExportRANFilesForm.StudyDataHasChanged (AContext: TChangeContext;
                                          AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TRainfallExportRANFilesForm.StudyDataHasChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallExportRANFilesForm.FormClose (Sender     : TObject;
                                              var Action : TCloseAction);
const OPNAME = 'TRainfallExportRANFilesForm.FormClose';
begin
  try
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallExportRANFilesForm.PopulateControls;
const OPNAME = 'TRainfallExportRANFilesForm.PopulateControls';
var
  lIndex                        : integer;
  LUpdateUser                   : boolean;
  lRainfallObj                  : IRainfallModelData;
  LCatchmentOutputFileData      : TCatchmentOutputFileData;
  LRainfallAsPercentMAP         : TRainfallAsPercentMAP;
begin
  try
    LUpdateUser := (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    LUpdateUser := LUpdateUser and (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));

    FBtnOK.Enabled     := LUpdateUser;
    FBtnCancel.Enabled := LUpdateUser;

    lRainfallObj := (FAppModules.Model.ModelData as IRainfallModelData);
    FDirectory   := lRainfallObj.DefaultDir;
    if (Trim(FDirectory) = '') then
      FDirectory := GetAppDataLocalDir+'\WRCDATA\'; //ExtractFilePath(ApplicationExeName) + 'wrcdata\';
    FLbxDirectory.Directory := FDirectory;

    if FCatchmentZone <> nil then
    begin
      LCatchmentOutputFileData := FCatchmentZone.CatchmentOutputFileData;
      if LCatchmentOutputFileData <> nil then
      begin
        lIndex := 0;
        LRainfallAsPercentMAP := LCatchmentOutputFileData.RainfallAsPercentMAPByIndex[lIndex];
        if(LRainfallAsPercentMAP <> nil)then
        begin
          FStartYear  := LRainfallAsPercentMAP.HydroYear;
          lIndex := LCatchmentOutputFileData.RainfallAsPercentMAPCount -1;

          LRainfallAsPercentMAP := LCatchmentOutputFileData.RainfallAsPercentMAPByIndex[lIndex];
          FEndYear  :=LRainfallAsPercentMAP.HydroYear;
        end;
      end;
    end;
    FCbxStartYear.Clear;
    FCbxEndYear.Clear;
    for lIndex := FStartYear to FEndYear do
    begin
      FCbxStartYear.Items.Add(IntToStr(lIndex));
      FCbxEndYear.Items.Add(IntToStr(lIndex));
    end;
    FCbxStartYear.ItemIndex := 0;
    FCbxEndYear.ItemIndex   := FCbxEndYear.Items.Count - 1;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallExportRANFilesForm.InputValid : Boolean;
const OPNAME = 'TRainfallExportRANFilesForm.InputValid';
begin
  Result := FALSE;
  try
    Result := TRUE;
    if (FCbxStartYear.ItemIndex >= 0) then
      FStartYear := StrToInt(FCbxStartYear.Text)
    else
    begin
      Result := FALSE;
      ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectStartYear'));
    end;
    if (Result) then
    begin
      if (FCbxEndYear.ItemIndex >= 0) then
        FEndYear := StrToInt(FCbxEndYear.Text)
      else
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectEndYear'));
      end;
    end;
    if (Result) then
    begin
      if (FEndYear < FStartYear) then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('Rainfall.EndYearBeforeStartYear'));
      end;
    end;
    if (Result) then
    begin
      if (Trim(FDirectory) = '') OR (NOT SysUtils.DirectoryExists(FDirectory)) then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('Rainfall.InvalidOutputDirectory'));
      end
      else
      begin
        if (Copy(FDirectory, Length(FDirectory), 1) <> '\') then
          FDirectory := FDirectory + '\';
      end;
    end;
    if (Result) then
    begin
      if (not FRANChkBox.Checked) and (not FOutputChkBox.Checked)then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('Rainfall.ChooseFile'));
      end;

    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallExportRANFilesForm.OnOKClick(Sender: TObject);
const OPNAME = 'TRainfallExportRANFilesForm.OnOKClick';
var
  LRainfallObj : TDataObjectRainfall;
begin
  try
    if (InputValid) then
    begin
      LRainfallObj := TDataObjectRainfall(FAppModules.Model.ModelData);
      LRainfallObj.WriteCatchmentToFile(FCatchmentZone,
                                         FDirectory,
                                         FRANChkBox.Checked  ,
                                         FOutputChkBox.Checked,
                                         FCbxStartYear.ItemIndex,
                                         FCbxEndYear.ItemIndex);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallExportRANFilesForm.OnCancelClick(Sender: TObject);
const OPNAME = 'TRainfallExportRANFilesForm.OnCancelClick';
begin
  try
    ModalResult := mrCancel;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallExportRANFilesForm.OnDirectoryChange (Sender: TObject);
const OPNAME = 'TRainfallExportRANFilesForm.OnDirectoryChange';
begin
  try
    FDirectory                 := FLbxDirectory.Directory;
    FLblDirectoryValue.Caption := FDirectory;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
